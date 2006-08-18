/*
 *  wings_fbx.cxx --
 *
 *     Erlang driver for FBX file import/export.
 *
 *  Copyright (c) 2003-2005 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_fbx.cxx,v 1.9 2005/03/16 20:26:15 bjorng Exp $
 */


#define K_NODLL
#include <fbxsdk.h>

#include <kfbxio/kfbxexporter.h>
#include <kfbxio/kfbximporter.h>
#include <kfbxplugins/kfbxsdkmanager.h>
#include <kfbxplugins/kfbxscene.h>

#include <stdio.h>

using namespace FBXSDK_NAMESPACE;

#include "fbx_ops.h"

extern "C" void send_response(char** res, char* s, int len);

// Forward declarations.

static bool SaveScene(KFbxSdkManager* pSdkManager,
                      KFbxScene* pScene, char* pFilename);
static void LoadScene(char** res, char* pFilename);

// Internal functions.
static double get_float(char* buff);
static KFbxVector4 get_vector(char*& buff);
static KFbxColor get_color(char*& buff);

static void send_error(char** res, KFbxImporter* importer);
static void send_floats(char** res, double* floats, int num_floats);
static void send_float(char** res, double f);
static void send_integer(char** res, int integer, int resp=RespInteger);
static void send_string(char** res, char* s);
static void send_color(char** res, KFbxColor& color);
static void send_mapping_mode(char** res, kInt mapping_mode);
static void send_reference_mode(char** res, kInt ref_mode);
static void send_bool(char** res, int b);

// Global variables.
static KFbxSdkManager* SdkManager = NULL;
static KFbxScene* Scene = NULL;
static int num_objects = 0;
static KFbxNode** objects = NULL;

// Current mesh.
static KFbxMesh* Mesh;          // Mesh itself.
static KFbxLayer* Layer;	// Layer 0.
static KFbxLayerElement* layerElem;	   // Current layer element.
static KFbxLayerElementMaterial* matLayer; // In layer 0 for current mesh.
static KFbxLayerElementVertexColor* colorLayer; // In layer 0 for current mesh.
static KFbxLayerElementTexture* texLayer; // In layer 0 for current mesh.
static KFbxLayerElementUV* uvLayer; // In layer 0 for current mesh.
static KFbxVector4* Points;     // Control points or normals.
static KFbxVector2* UVs;        // UV coordinates.

static kInt* MaterialIndices;   // Material indices (for import).
static kInt* TextureIndices;    // Texture indices (for import).

// Current material and texture.
static KFbxMaterial* Material;
static KFbxTexture* Texture;

// Current node.
static KFbxNode* Node;

// Current light.
static KFbxLight* Light;

// Version of file
static int Major, Minor, Revision;

// File format;
static KFbxExporter::EFileFormat FileFormat;

struct StackNode {
  KFbxNode* node;
  StackNode* next;
};

static StackNode* Stack = 0;

extern "C" int
fbx_control(unsigned int command, 
            char* buff, int count,
            char** res, int res_size)
{
  switch (command) {
  case ExpInitialize:
    SdkManager = KFbxSdkManager::CreateKFbxSdkManager();
    Scene = SdkManager->CreateKFbxScene();
    objects = 0;
    num_objects = 0;
    break;
  case ExpDestroy:
    delete [] objects;
    delete SdkManager;
    objects = 0;
    num_objects = 0;
    break;
  case ExpNumObjects:
    num_objects = *(unsigned *) buff;
    objects = new KFbxNode*[num_objects];
    num_objects = 0;
    break;
  case ExpCreateMesh:
    {
      Mesh = SdkManager->CreateKFbxMesh();
      if (Mesh->GetLayer(0) == NULL) {
	Mesh->CreateLayer();
      }
      Layer = Mesh->GetLayer(0);

      matLayer = new KFbxLayerElementMaterial;
      matLayer->SetMappingMode(KFbxLayerElement::eBY_POLYGON);
      matLayer->SetReferenceMode(KFbxLayerElement::eINDEX_TO_DIRECT);
      Layer->SetMaterials(matLayer);
    }
    break;
  case ExpInitControlPoints:
    {
      unsigned n = *(unsigned *) buff;
      Mesh->InitControlPoints(n);
      Points = Mesh->GetControlPoints();
    }
    break;
  case ExpInitNormals:
    {
      Mesh->InitNormals();
      Points = Mesh->GetNormals();
    }
    break;
  case ExpPoint:
    {
      KFbxVector4 lPoint = get_vector(buff);
      *Points++ = lPoint;
    }
    break;
  case ExpInitUVs:
    {
      unsigned n = *(unsigned *) buff;
      Mesh->InitTextureUVIndices(KFbxLayerElement::eBY_POLYGON_VERTEX);
      Mesh->InitTextureUV(n);
      UVs = Mesh->GetTextureUV();
    }
    break;
  case ExpUV:
    {
      double u, v;
      u = *(double *) buff; buff += sizeof(double);
      v = *(double *) buff; buff += sizeof(double);
      UVs->Set(u, v);
      UVs++;
    }
    break;
  case ExpBeginPolygon:
    {
      kInt material = *(unsigned *) buff; buff += sizeof(unsigned);
      kInt texture = *(unsigned *) buff; buff += sizeof(unsigned);
      Mesh->BeginPolygon(material, texture);
    }
    break;
  case ExpAddPolygon:
    {
      kInt vindex = *(unsigned *) buff; buff += sizeof(unsigned);
      kInt tindex = *(unsigned *) buff; buff += sizeof(unsigned);
      Mesh->AddPolygon(vindex, tindex);
    }
    break;
  case ExpEndPolygon:
    Mesh->EndPolygon();
    break;
  case ExpCreateNode:
    {
      Node = SdkManager->CreateKFbxNode(buff);
      objects[num_objects++] = Node;
    }
    break;
  case ExpAssignMesh:            // Assign mesh to current node.
    Node->SetNodeAttribute(Mesh);
    break;
  case ExpSetShadingMode:
    {
      unsigned shading = *(unsigned *) buff;
      KFbxNode::EShadingMode sh;
      switch (shading) {
      case EnumHardShading:
        sh = KFbxNode::eHARD_SHADING;
        break;
      case EnumWireFrame:
        sh = KFbxNode::eWIRE_FRAME;
        break;
      case EnumFlatShading:
        sh = KFbxNode::eFLAT_SHADING;
        break;
      case EnumLightShading:
        sh = KFbxNode::eLIGHT_SHADING;
        break;
      case EnumTextureShading:
        sh = KFbxNode::eTEXTURE_SHADING;
        break;
      case EnumLightTextureShading:
        sh = KFbxNode::eLIGHT_TEXTURE_SHADING;
        break;
      }
      Node->SetShadingMode(sh);
    }
    break;
  case ExpSetDefaultT:
    {
      KFbxVector4 DefT = get_vector(buff);
      Node->SetDefaultT(DefT);
    }
    break;
  case ExpSetDefaultR:
    {
      KFbxVector4 DefR = get_vector(buff);
      Node->SetDefaultR(DefR);
    }
    break;
  case ExpMaterial:
    {
      Material = SdkManager->CreateKFbxMaterial(buff);
      Material->SetShadingModel("phong");
      int m = matLayer->GetDirectArray().Add(Material);
      matLayer->GetIndexArray().Add(m);
    }  
    break;
  case ExpAmbient:
    Material->SetAmbient(get_color(buff));
    break;
  case ExpDiffuse:
    Material->SetDiffuse(get_color(buff));
    break;
  case ExpSpecular:
    Material->SetSpecular(get_color(buff));
    break;
  case ExpEmissive:
    Material->SetEmissive(get_color(buff));
    break;
  case ExpOpacity:
    Material->SetOpacity(get_float(buff));
    break;
  case ExpShininess:
    Material->SetShininess(get_float(buff));
    break;

  case ExpInitTextures:
    Mesh->InitTextureIndices(KFbxLayerElement::eBY_POLYGON);
    break;
  case ExpCreateTexture:
    {
      Texture = SdkManager->CreateKFbxTexture(buff);
      
      Texture->SetTextureUse(KFbxTexture::eSTANDARD);
      Texture->SetMappingType(KFbxTexture::eUV);
      Texture->SetMaterialUse(KFbxTexture::eMODEL_MATERIAL);
      Texture->SetSwapUV(false);
      Texture->SetTranslation(0.0, 0.0);
      Texture->SetScale(1.0, 1.0);
      Texture->SetRotation(0.0, 0.0);
      KFbxLayerElementTexture* texLayer = Layer->GetTextures();
      int m = texLayer->GetDirectArray().Add(Texture);
      texLayer->GetIndexArray().Add(m);
    }
    break;
  case ExpTextureFilename:
    Texture->SetFileName(buff);
    break;

  case ExpInitVertexColorTable:
    colorLayer = new KFbxLayerElementVertexColor;
    colorLayer->SetMappingMode(KFbxLayerElement::eBY_POLYGON_VERTEX);
    colorLayer->SetReferenceMode(KFbxLayerElement::eINDEX_TO_DIRECT);
    Layer->SetVertexColors(colorLayer);
    break;

  case ExpVertexColor:
    {
      KFbxColor col = get_color(buff);
      colorLayer->GetDirectArray().Add(col);
    }
    break;

  case ExpVertexColorIndex:
    {
      unsigned index = *(unsigned *) buff;
      colorLayer->GetIndexArray().Add(index);
    }
    break;

    //
    // Lights.
    //
  case ExpCreateLight:
    Light = SdkManager->CreateKFbxLight();
    break;
  case ExpLightType:
    {
      unsigned light_type = *(unsigned *) buff;
      KFbxLight::ELightType lt;
      switch (light_type) {
      case LightPoint:
        lt = KFbxLight::ePOINT;
        break;
      case LightDirectional:
        lt = KFbxLight::eDIRECTIONAL;
        break;
      case LightSpot:
        lt = KFbxLight::eSPOT;
        break;
      }
      Light->SetLightType(lt);
    }
    break;
  case ExpLightColor:
    {
      KFbxColor col = get_color(buff);
      Light->SetDefaultColor(col);
    }
    break;
  case ExpLightConeAngle:
    {
      double angle = *(double *) buff;
      Light->SetDefaultConeAngle(angle);
    }
    break;
  case ExpAssignLight:            // Assign light to current node.
    Node->SetNodeAttribute(Light);
    break;
  case ExpAmbientColor:
    Scene->GetGlobalLightSettings().SetAmbientColor(get_color(buff));
    break;
  case ExpAxisAlignmentInEulerAngle:
    {
      KFbxVector4 pAB;
      KFbxVector4 pA;
      KFbxVector4 pB;
      KFbxVector4 pAngles;

      pAB = get_vector(buff);
      pA = get_vector(buff);
      pB = get_vector(buff);
      KFbxVector4::AxisAlignmentInEulerAngle(pAB, pA, pB, pAngles);
      send_floats(res, (kDouble *) pAngles, 3);
      break;
    }
  case ExpLinkObjects:
    {
      KFbxNode* lRootNode = Scene->GetRootNode();
      for (int i = 0; i < num_objects; i++) {
        KFbxNode* node = objects[i];
        lRootNode->AddChild(objects[i]);
      }
    }
    break;
  case ExpSetFileFormat:
    switch (*buff) {
    case ExpFileFormatFbx5:
      FileFormat = KFbxExporter::eFBX_50_BINARY;
      break;
    case ExpFileFormatFbx6:
      FileFormat = KFbxExporter::eFBX_BINARY;
      break;
    case ExpFileFormatFbx5Ascii:
      FileFormat = KFbxExporter::eFBX_50_ASCII;
      break;
    case ExpFileFormatFbx6Ascii:
      FileFormat = KFbxExporter::eFBX_ASCII;
      break;
    }
    break;
  case ExpSaveScene:
    SaveScene(SdkManager, Scene, buff);
    break;

    //
    // Import functions start here.
    //
  case ImpLoadScene:
    LoadScene(res, buff);
    Node = Scene->GetRootNode();
    break;
  case ImpVersion:
    {
      char sbuf[64];
      
      sprintf(sbuf, "%d.%d.%d", Major, Minor, Revision);
      send_string(res, sbuf);
      break;
    }

  case ImpGlobalAmbient:
    {
      double rgb[3];
      KFbxColor amb = Scene->GetGlobalLightSettings().GetAmbientColor();
      rgb[0] = amb.mRed;
      rgb[1] = amb.mGreen;
      rgb[2] = amb.mBlue;
      send_floats(res, rgb, 3);
    }
    break;
  case ImpNumChildren:
    {
      int children = Node->GetChildCount();
      send_integer(res, children);
    }
    break;
  case ImpEnterChildNode:
    {
      kInt child = *(kInt *) buff;
      StackNode* stk_node = new StackNode;
      stk_node->node = Node;
      stk_node->next = Stack;
      Stack = stk_node;
      Node = Node->GetChild(child);
    }
    break;
  case ImpPopNode:
    {
      StackNode* stk_node = Stack;
      Node = Stack->node;
      Stack = stk_node->next;
      delete stk_node;
    }
    break;
  case ImpNodeType:
    {
      KFbxNodeAttribute* lNodeAttribute = Node->GetNodeAttribute();
      if (lNodeAttribute == 0) {
        send_integer(res, NodeEmpty);
      } else {
        int type = 0;
        switch (lNodeAttribute->GetAttributeType()) {
        case KFbxNodeAttribute::eNULL:
          type = NodeNull;
          break;
        case KFbxNodeAttribute::eMARKER:
          type = NodeMarker;
          break;
        case KFbxNodeAttribute::eSKELETON:
          type = NodeSkeleton;
          break;
        case KFbxNodeAttribute::eMESH: 
          type = NodeMesh;
          break;
        case KFbxNodeAttribute::eNURB:
          type = NodeNurb;
          break;
        case KFbxNodeAttribute::ePATCH:
          type = NodePatch;
          break;
        case KFbxNodeAttribute::eCAMERA:
          type = NodeCamera;
          break;
        case KFbxNodeAttribute::eCAMERA_SWITCHER:
          type = NodeCameraSwitcher;
          break;
        case KFbxNodeAttribute::eLIGHT:
          type = NodeLight;
          break;
        case KFbxNodeAttribute::eOPTICAL_REFERENCE:
          type = NodeOpticalReference;
          break;
        case KFbxNodeAttribute::eOPTICAL_MARKER:
          type = NodeOpticalMarker;
          break;
	default:
	  type = NodeUnknown;
	  break;
        }
        send_integer(res, type);
      }
    }
    break;
  case ImpNodeName:
    send_string(res, Node->GetName());
    break;
  case ImpIsVisible:
    send_bool(res, Node->GetVisibility());
    break;
  case ImpDefaultT:
    {
      KFbxVector4 vec;
      Node->GetDefaultT(vec);
      send_floats(res, vec.mData, 3);
    }
    break;
  case ImpDefaultR:
    {
      KFbxVector4 vec;
      Node->GetDefaultR(vec);
      send_floats(res, vec.mData, 3);
    }
    break;
  case ImpDefaultS:
    {
      KFbxVector4 vec;
      Node->GetDefaultS(vec);
      send_floats(res, vec.mData, 3);
    }
    break;
  case ImpMesh:
    Mesh = (KFbxMesh*) Node->GetNodeAttribute();
    Layer = Mesh->GetLayer(0);
    break;
  case ImpConvertToMesh:
    {
      KFbxGeometryConverter* lConverter =
        SdkManager->CreateKFbxGeometryConverter();
      
      lConverter->TriangulateInPlace(Node);
      SdkManager->DestroyKFbxGeometryConverter(lConverter);
    }
    break;
  case ImpNumVertices:
    send_integer(res, Mesh->GetControlPointsCount());
    break;
  case ImpVertices:
    Points = Mesh->GetControlPoints();
    break;
  case ImpNormals:
    Points = Mesh->GetNormals();
    break;
  case ImpUVs:
    UVs = Mesh->GetTextureUV();
    break;
  case ImpPoint:
    send_floats(res, Points->mData, 3);
    Points++;
    break;
  case ImpUV:
    send_floats(res, UVs->mData, 2);
    UVs++;
    break;
  case ImpNumPolygons:
    send_integer(res, Mesh->GetPolygonCount());
    break;
  case ImpPolygonSize:
    {
      int i = *(int *) buff;
      send_integer(res, Mesh->GetPolygonSize(i));
    }
    break;
  case ImpPolygonVertex:
    {
      int i = *(int *) buff; buff += sizeof(int);
      int j = *(int *) buff; buff += sizeof(int);
      send_integer(res, Mesh->GetPolygonVertex(i, j));
    }
    break;
  case ImpPolygonUV:
    {
      int i = *(int *) buff; buff += sizeof(int);
      int j = *(int *) buff; buff += sizeof(int);
      send_integer(res, Mesh->GetTextureUVIndex(i, j));
    }
    break;
  case ImpLocalTransformation:
    {
      KFbxVector4 T, R, S, Tmp;
      KFbxXMatrix LocalPosition;
      double matrix[16];

      KFbxTakeNode* lCurrentTakeNode = Node->GetCurrentTakeNode();
      KFbxTakeInfo* lCurrentTakeInfo = Scene->GetTakeInfo(lCurrentTakeNode->GetName());
      KTime pTime;

      if (lCurrentTakeInfo) {
        pTime = lCurrentTakeInfo->mLocalTimeSpan.GetStart();
      } else {
        pTime = KTIME_ZERO;
      }

      T[0] = lCurrentTakeNode->GetTranslationX()->Evaluate(pTime);
      T[1] = lCurrentTakeNode->GetTranslationY()->Evaluate(pTime);
      T[2] = lCurrentTakeNode->GetTranslationZ()->Evaluate(pTime);
      LocalPosition.SetT(T);

      R[0] = lCurrentTakeNode->GetEulerRotationX()->Evaluate(pTime);
      R[1] = lCurrentTakeNode->GetEulerRotationY()->Evaluate(pTime);
      R[2] = lCurrentTakeNode->GetEulerRotationZ()->Evaluate(pTime);
      LocalPosition.SetR(R);

      S[0] = lCurrentTakeNode->GetScaleX()->Evaluate(pTime);
      S[1] = lCurrentTakeNode->GetScaleY()->Evaluate(pTime);
      S[2] = lCurrentTakeNode->GetScaleZ()->Evaluate(pTime);
      LocalPosition.SetS(S);
#if 0
      Node->GetDefaultT(T);
      Node->GetDefaultR(R);
      Node->GetDefaultS(S);
      LocalPosition.SetT(T);
      LocalPosition.SetR(R);
      LocalPosition.SetS(S);
#endif
      send_floats(res, (double*) LocalPosition, 16);
    }
    break;
  case ImpNumLinks:
    send_integer(res, Mesh->GetLinkCount());
    break;
  case ImpNumShapes:
    send_integer(res, Mesh->GetShapeCount());
    break;

    //
    // General layer element support.
    //
  case ImpMappingMode:
    send_mapping_mode(res, layerElem->GetMappingMode());
    break;
  case ImpReferenceMode:
    send_reference_mode(res, layerElem->GetReferenceMode());
    break;

    //
    // Material import.
    //

  case ImpInitMaterials:
    {
      unsigned b = 0;

      if (Layer) {
	layerElem = matLayer = Layer->GetMaterials();
	b = (layerElem != NULL);
      }
      send_bool(res, b);
    }
    break;
  case ImpMaterialIndices:
    MaterialIndices = Mesh->GetMaterialIndices();
    break;
  case ImpMaterialIndex:
    send_integer(res, *MaterialIndices++);
    break;
  case ImpNumMaterials:
    switch (layerElem->GetReferenceMode()) {
    case KFbxLayerElement::eDIRECT:
    case KFbxLayerElement::eINDEX_TO_DIRECT:
      send_integer(res, matLayer->GetDirectArray().GetCount());
      break;
    case KFbxLayerElement::eINDEX:
      send_integer(res, matLayer->GetIndexArray().GetCount());
      break;
    default:
      send_integer(res, 0);
      break;
    }
    break;
  case ImpMaterial:
    {
      kInt index = *(kInt *) buff;
      Material = matLayer->GetDirectArray().GetAt(index);
    }
    break;
  case ImpMaterialName:
    send_string(res, Material->GetName());
    break;
  case ImpAmbient:
    {
      KFbxColor col = Material->GetAmbient();
      send_color(res, col);
    }
    break;
  case ImpDiffuse:
    {
      KFbxColor col = Material->GetDiffuse();
      send_color(res, col);
    }
    break;
  case ImpSpecular:
    {
      KFbxColor col = Material->GetSpecular();
      send_color(res, col);
    }
    break;
  case ImpEmissive:
    {
      KFbxColor col = Material->GetEmissive();
      send_color(res, col);
    }
    break;
  case ImpShininess:
    send_float(res, Material->GetShininess());
    break;
  case ImpOpacity:
    send_float(res, Material->GetOpacity());
    break;

    //
    // Importing of UVs.
    //

  case ImpInitUVs:
    {
      unsigned b = 0;

      if (Layer) {
	layerElem = uvLayer = Layer->GetUVs();
	if (uvLayer != NULL) {
	  // There is a UV layer element. Unfortunately, careless
	  // exporters (such as previous releases of this plug-in)
	  // could create the layer element without filling it in.
	  // Therefore, check that the needed arrays are really there.
	  switch (layerElem->GetReferenceMode()) {
	  case KFbxLayerElement::eDIRECT:
	    b = uvLayer->GetDirectArray() != NULL;
	    break;
	  case KFbxLayerElement::eINDEX:
	    b = uvLayer->GetIndexArray() != NULL;
	    break;
	  case KFbxLayerElement::eINDEX_TO_DIRECT:
	    b = uvLayer->GetDirectArray() != NULL &&
	      uvLayer->GetIndexArray() != NULL;
	    break;
	  }
	}
      }
      send_bool(res, b);
    }
    break;
  case ImpNumUVs:
    send_integer(res, Mesh->GetTextureUVCount());
    break;

    //
    // Importing of textures.
    //

  case ImpInitTextures:
    {
      unsigned b = 0;

      if (Layer) {
	layerElem = texLayer = Layer->GetTextures();
	if (texLayer != NULL) {
	  // There is a texture layer element. Unfortunately, careless
	  // exporters (such as previous releases of this plug-in)
	  // could create the layer element without filling it in.
	  // Therefore, check that the needed arrays really are there.
	  switch (layerElem->GetReferenceMode()) {
	  case KFbxLayerElement::eDIRECT:
	    b = texLayer->GetDirectArray() != NULL;
	    break;
	  case KFbxLayerElement::eINDEX:
	    b = texLayer->GetIndexArray() != NULL;
	    break;
	  case KFbxLayerElement::eINDEX_TO_DIRECT:
	    b = texLayer->GetDirectArray() != NULL &&
	      texLayer->GetIndexArray() != NULL;
	    break;
	  }
	}
	send_bool(res, b);
      }
    }
    break;
  case ImpNumTextures:
    send_integer(res, texLayer->GetDirectArray().GetCount());
    break;
  case ImpTexture:
    {
      kInt index = *(kInt *) buff;
      Texture = texLayer->GetDirectArray().GetAt(index);
    }
    break;
  case ImpTextureFileName:
    send_string(res, Texture->GetFileName());
    break;
  case ImpTxMappingType:
    {
      int type;
      switch (Texture->GetMappingType()) {
      case KFbxTexture::eNULL:
        type = TxMapNull; break;
      case KFbxTexture::ePLANAR:
        type = TxMapPlanar; break;
      case KFbxTexture::eSPHERICAL:
        type = TxMapSpherical; break;
      case KFbxTexture::eCYLINDRICAL:
        type = TxMapCylindrical; break;
      case KFbxTexture::eBOX:
        type = TxMapBox; break;
      case KFbxTexture::eFACE:
        type = TxMapFace; break;
      case KFbxTexture::eUV:
        type = TxMapUV; break;
      case KFbxTexture::eENVIRONMENT:
        type = TxMapEnvironment; break;
      }
      send_integer(res, type);
    }
    break;
  case ImpTextureIndices:
    TextureIndices = Mesh->GetTextureIndices();
    break;
  case ImpTextureIndex:
    send_integer(res, *TextureIndices++);
    break;
  case ImpLight:
    Light = (KFbxLight*) Node->GetNodeAttribute();
    break;
  case ImpLightType:
    {
      unsigned light_type;
      switch (Light->GetLightType()) {
      case KFbxLight::ePOINT:
        light_type = LightPoint;
        break;
      case KFbxLight::eDIRECTIONAL:
        light_type = LightDirectional;
        break;
      case KFbxLight::eSPOT:
        light_type = LightSpot;
        break;
      }
      send_integer(res, light_type);
      break;
    }
  case ImpLightColor:
    {
      KFbxColor color;
      Light->GetDefaultColor(color);
      send_color(res, color);
    }
    break;
  case ImpConeAngle:
    send_float(res, Light->GetDefaultConeAngle());
    break;

    // Vertex colors.

  case ImpInitVertexColors:
    {
      unsigned b = 0;

      if (Layer) {
	layerElem = colorLayer = Layer->GetVertexColors();
	b = (layerElem != NULL);
      }
      send_bool(res, b);
    }
    break;

  default:
    return -1;
  }
  return 0;
}

static double
get_float(char* buff)
{
  return *(double *) buff;
}

static KFbxVector4
get_vector(char*& buff)
{
  double x, y, z;
  x = *(double *) buff; buff += sizeof(double);
  y = *(double *) buff; buff += sizeof(double);
  z = *(double *) buff; buff += sizeof(double);
  return KFbxVector4(x, y, z);
}

static KFbxColor
get_color(char*& buff)
{
  double r, g, b;

  r = *(double *) buff; buff += sizeof(double);
  g = *(double *) buff; buff += sizeof(double);
  b = *(double *) buff; buff += sizeof(double);
  return KFbxColor(r, g, b);
}

static void
send_error(char** res, KFbxImporter* importer)
{
  if (importer->GetLastErrorID() == KFbxIO::ePASSWORD_ERROR) {
    char sbuf[1] = {RespPasswordProtected};
    send_response(res, sbuf, 1);
  } else {
    char* error = importer->GetLastErrorString();
    char* sbuf = new char[strlen(error)+2];

    sbuf[0] = RespError;
    sprintf(sbuf+1, error);
    send_response(res, sbuf, 1+strlen(sbuf+1));
    delete sbuf;
  }
}

static void
send_color(char** res, KFbxColor& color)
{
  double f[3];

  f[0] = color.mRed;
  f[1] = color.mGreen;
  f[2] = color.mBlue;
  send_floats(res, f, 3);
}

static void
send_floats(char** res, double* floats, int num_floats)
{
  int size = 1+num_floats*sizeof(double);
  char* sbuf = new char[size];
  
  sbuf[0] = RespFloat;
  memcpy(sbuf+1, floats, num_floats*sizeof(double));
  send_response(res, sbuf, size);
  delete sbuf;
}

static void
send_float(char** res, double f)
{
  char sbuf[1+sizeof(double)];

  sbuf[0] = RespFloat;
  memcpy(sbuf+1, &f, sizeof(double));
  send_response(res, sbuf, 1+sizeof(double));
}

static void
send_integer(char** res, int integer, int resp)
{
  char sbuf[1+sizeof(int)];

  sbuf[0] = resp;
  memcpy(sbuf+1, &integer, sizeof(int));
  send_response(res, sbuf, 1+sizeof(int));
}

static void
send_string(char** res, char* s)
{
  int size = strlen(s);
  char* sbuf = new char[1+size];

  sbuf[0] = RespString;
  memcpy(sbuf+1, s, size);
  send_response(res, sbuf, 1+size);
  delete sbuf;
}

static void
send_mapping_mode(char** res, kInt mapping_mode)
{
  int type = MappingByControlPoint;

  switch (mapping_mode) {
  case KFbxLayerElement::eBY_CONTROL_POINT:
    type = MappingByControlPoint;
    break;
  case KFbxLayerElement::eBY_POLYGON_VERTEX:
    type = MappingByPolygonVertex;
    break;
  case KFbxLayerElement::eBY_POLYGON:
    type = MappingByPolygon;
    break;
  case KFbxLayerElement::eALL_SAME:
    type = MappingAllSame;
    break;
  }
  send_integer(res, type, RespMappingMode);
}

static void
send_reference_mode(char** res, kInt ref_mode)
{
  int type = RefModeDirect;

  switch (ref_mode) {
  case KFbxLayerElement::eDIRECT:
    type = RefModeDirect;
    break;
  case KFbxLayerElement::eINDEX:
    type = RefModeIndex;
    break;
  case KFbxLayerElement::eINDEX_TO_DIRECT:
    type = RefIndexToDirect;
    break;
  }
  send_integer(res, type, RespReferenceMode);
}

static void
send_bool(char** res, int b)
{
  char sbuf[2];

  sbuf[0] = RespBoolean;
  sbuf[1] = b;
  send_response(res, sbuf, sizeof(sbuf));
}

static bool
SaveScene(KFbxSdkManager* pSdkManager, KFbxScene* pScene,
          char* pFilename)
{
  int lMajor, lMinor, lRevision;

  // Create an exporter.
  KFbxExporter* lExporter = pSdkManager->CreateKFbxExporter();
  char* password = pFilename + strlen(pFilename) + 1;

  lExporter->SetFileFormat(FileFormat);

  // Initialize the exporter by providing a filename.
  if (lExporter->Initialize(pFilename) == false) {
    printf("Call to KFbxExporter::Initialize() failed.\n");
    printf("Error returned: %s\n\n", lExporter->GetLastErrorString());
    return false;
  }

  lExporter->SetPassword(password);
	
  KFbxIO::GetCurrentVersion(lMajor, lMinor, lRevision);
	
  // Set the export states. By default, the export states are always set to 
  // true except for the option eEXPORT_TEXTURE_AS_EMBEDDED. The code below 
  // shows how to change these states.
  lExporter->SetState(KFbxExporter::eEXPORT_MATERIAL, true);
  lExporter->SetState(KFbxExporter::eEXPORT_TEXTURE, true);
  lExporter->SetState(KFbxExporter::eEXPORT_TEXTURE_AS_EMBEDDED, false);
  lExporter->SetState(KFbxExporter::eEXPORT_LINK, true);
  lExporter->SetState(KFbxExporter::eEXPORT_SHAPE, true);
  lExporter->SetState(KFbxExporter::eEXPORT_GOBO, true);
  lExporter->SetState(KFbxExporter::eEXPORT_ANIMATION, true);
  lExporter->SetState(KFbxExporter::eEXPORT_GLOBAL_SETTINGS, true);

  // Export the scene.
  bool lStatus = lExporter->Export(*pScene);   

  // Destroy the exporter.
  pSdkManager->DestroyKFbxExporter(lExporter);

  return lStatus;
}

static void
LoadScene(char **res, char* pFilename)
{
  KFbxImporter* lImporter = SdkManager->CreateKFbxImporter();
  char* password = pFilename + strlen(pFilename) + 1;

  // Initialize the importer by providing a filename.
  if (lImporter->Initialize(pFilename) == false) {
    send_error(res, lImporter);
    SdkManager->DestroyKFbxImporter(lImporter);
  } else {
    lImporter->GetFileVersion(Major, Minor, Revision);

    lImporter->SetPassword(password);

    // Set the import states. To possibly save some time,
    // we don't import things such as animations which are not
    // supported in Wings anyway.
    lImporter->SetState(KFbxImporter::eIMPORT_MATERIAL, true);
    lImporter->SetState(KFbxImporter::eIMPORT_TEXTURE, true);
    lImporter->SetState(KFbxImporter::eIMPORT_LINK, true);
    lImporter->SetState(KFbxImporter::eIMPORT_SHAPE, true);
    lImporter->SetState(KFbxImporter::eIMPORT_GOBO, false);
    lImporter->SetState(KFbxImporter::eIMPORT_ANIMATION, true);
    lImporter->SetState(KFbxImporter::eIMPORT_GLOBAL_SETTINGS, true);

    // Import the scene.
    if (!lImporter->Import(*Scene)) {
      send_error(res, lImporter);
    }

    // Destroy the importer.
    SdkManager->DestroyKFbxImporter(lImporter);
  }
}
