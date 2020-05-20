/*****************************************************************************/
/* kernels for bvh					                     */
/*							                     */
/* Mostly stolen from Luxrender guys all credit to them	                     */
/*							                     */
/*  See the file "license.terms" for information on usage and redistribution */
/*  of this file, and for a DISCLAIMER OF ALL WARRANTIES.                    */
/*****************************************************************************/

#define IsLeaf(nodeData) ((nodeData) & 0x80000000u)
#define GetSkipIndex(nodeData) ((nodeData) & 0x7fffffffu)

typedef struct {
  union {
    struct {
      float bboxMin[3];
      float bboxMax[3];
    } bvhNode;
    struct {
      unsigned int v[3];
      unsigned int meshIndex, triangleIndex;
    } triangleLeaf;
  };
  // Most significant bit is used to mark leafs
  unsigned int nodeData;
  int pad0; // To align to float4
} TreeNode;

typedef struct {
  float x, y, z;
} Point;

typedef struct {
  Point o;
  Point d;
  float mint, maxt;
} Ray;

typedef struct {
  float t, b1, b2;
  unsigned int meshIndex, triangleIndex;
} RayHit;

float3 make_float3(const __global float *p)
{
  return (float3)(p[0], p[1], p[2]);
}

void make_ray(__global const Ray* restrict ray, Ray *dstRay)
{
  __global float4 *basePtr =(__global float4 *)ray;
  const float4 data0 = (*basePtr++);
  const float4 data1 = (*basePtr);

  dstRay->o.x = data0.x;
  dstRay->o.y = data0.y;
  dstRay->o.z = data0.z;
  dstRay->d.x = data0.w;
  dstRay->d.y = data1.x;
  dstRay->d.z = data1.y;

  dstRay->mint = data1.z;
  dstRay->maxt = data1.w;

}

int bbox_ray(const float3 pMin, const float3 pMax,
	     const float3 rayOrig, const float3 invRayDir,
	     const float mint, const float maxt)
{
  const float3 l1 = (pMin - rayOrig) * invRayDir;
  const float3 l2 = (pMax - rayOrig) * invRayDir;
  const float3 tNear = fmin(l1, l2);
  const float3 tFar = fmax(l1, l2);

  float t0 = fmax(fmax(fmax(tNear.x, tNear.y), fmax(tNear.x, tNear.z)), mint);
  float t1 = fmin(fmin(fmin(tFar.x, tFar.y), fmin(tFar.x, tFar.z)), maxt);

  return (t1 > t0);
}

void tri_ray(const float3 rayOrig,   const float3 rayDir,  const float mint, float *maxt,
	     uint *hitMeshIndex,  uint *hitTriangleIndex,
	     float *hitB1, float *hitB2,
	     const uint currentMeshIndex,  const uint currentTriangleIndex,
	     const float3 v0, const float3 v1, const float3 v2)
{
  // Calculate intersection
  const float3 e1 = v1 - v0;
  const float3 e2 = v2 - v0;
  const float3 s1 = cross(rayDir, e2);

  const float divisor = dot(s1, e1);
  if (divisor == 0.f)
    return;

  const float invDivisor = 1.f / divisor;

  // Compute first barycentric coordinate
  const float3 d = rayOrig - v0;
  const float b1 = dot(d, s1) * invDivisor;
  if (b1 < 0.f)
    return;

  // Compute second barycentric coordinate
  const float3 s2 = cross(d, e1);
  const float b2 = dot(rayDir, s2) * invDivisor;
  if (b2 < 0.f)
    return;

  const float b0 = 1.f - b1 - b2;
  if (b0 < 0.f)
    return;

  // Compute _t_ to intersection point
  const float t = dot(e2, s2) * invDivisor;
  if (t < mint || t > *maxt)
    return;

  *maxt = t;
  *hitB1 = b1;
  *hitB2 = b2;
  *hitMeshIndex = currentMeshIndex;
  *hitTriangleIndex = currentTriangleIndex;
}


void bvh_ray(const Ray *ray,  RayHit *rayHit,
	     __global const Point* restrict verts, __global const TreeNode* restrict tree)
{
  const uint stopNode = GetSkipIndex(tree[0].nodeData); // Non-existent

  const float3 rayOrig = (float3)(ray->o.x, ray->o.y, ray->o.z);
  const float3 rayDir = (float3)(ray->d.x, ray->d.y, ray->d.z);
  const float mint = ray->mint;
  float maxt = ray->maxt;

  const float3 invRayDir = 1.f / rayDir;

  uint hitMeshIndex = 0xffffffffu;
  uint hitTriangleIndex = 0xffffffffu;
  uint currentNode = 0; // Root Node

  float b1, b2;
  while (currentNode < stopNode) {
    __global const TreeNode* restrict node = &tree[currentNode];
    // Read the node
    __global float4* restrict data = (__global float4* restrict)node;
    const float4 data0 = *data++;
    const float4 data1 = *data;

    const uint nodeData = as_uint(data1.s2);
    if (IsLeaf(nodeData)) {  // It is a leaf, check the triangle
      const uint v0 = as_uint(data0.s0);
      const uint v1 = as_uint(data0.s1);
      const uint v2 = as_uint(data0.s2);

      const float3 p0 = make_float3(&verts[v0].x);
      const float3 p1 = make_float3(&verts[v1].x);
      const float3 p2 = make_float3(&verts[v2].x);

      const uint meshIndex = as_uint(data0.s3);
      const uint triangleIndex = as_uint(data1.s0);

      tri_ray(rayOrig, rayDir, mint, &maxt, &hitMeshIndex, &hitTriangleIndex,
	      &b1, &b2, meshIndex, triangleIndex, p0, p1, p2);
      ++currentNode;
    } else {   // It is a node, check the bounding box
      const float3 pMin = (float3)(data0.s0, data0.s1, data0.s2);
      const float3 pMax = (float3)(data0.s3, data1.s0, data1.s1);

      if (bbox_ray(pMin, pMax, rayOrig, invRayDir, mint, maxt)) {
	++currentNode;
      } else {
	currentNode = nodeData;
      }
    }
  }

  rayHit->t = maxt;
  rayHit->b1 = b1;
  rayHit->b2 = b2;
  rayHit->meshIndex = hitMeshIndex;
  rayHit->triangleIndex = hitTriangleIndex;

}

__kernel __attribute__((work_group_size_hint(64, 1, 1)))
void bvh_rays(__global const Ray* restrict rays, __global RayHit *rayHits,
	      const uint rayCount,
	      __global const Point* restrict verts, __global const TreeNode* restrict tree)
{
  // Select the ray to check
  const int gid = get_global_id(0);
  if (gid >= rayCount)
    return;

  Ray ray;
  make_ray(&rays[gid], &ray);

  RayHit rayHit;
  bvh_ray(&ray, &rayHit, verts, tree);

  // Write result
  __global RayHit *memRayHit = &rayHits[gid];
  memRayHit->t = rayHit.t;
  memRayHit->b1 = rayHit.b1;
  memRayHit->b2 = rayHit.b2;
  memRayHit->meshIndex = rayHit.meshIndex;
  memRayHit->triangleIndex = rayHit.triangleIndex;
}
