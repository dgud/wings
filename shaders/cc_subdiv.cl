// -*- c++ -*-
// @Author: <dgud@users.sf.net>
// @copyright (C) 2010
// @doc Catmull Clark subdivision

typedef struct {
  int start;
  int len;
} FaceIndex;

void find_faces(int V0, int V1, FaceIndex Fi, __global int *Fs, 
		int * F1, int *F2, int *CCW);

__kernel void gen_faces(
			__global float4 *VsIn,
			__global int *FsIn,
			__global FaceIndex *FiIn,
			__global float4 *VsOut,
			__global int4 *FsOut,
			//__global int *locks,
			const uint noFs,
			const uint noVs
			)
{
  int i;
  const int face_id = get_global_id(0);
  if (face_id >= noFs)
      return;
  const FaceIndex fi = FiIn[face_id];
  float4 center = {0.0,0.0,0.0,0.0};
  
  for(i=0; i < fi.len; i++) {
      center.xyz += VsIn[FsIn[fi.start+i]].xyz;
  }

  center /= i;
  // Create new center vertex
  const uint ov_id = noVs + face_id;
  center.w = fi.len*4.0;  // Valance = faceVs and hard_edge count = 0 (Valance << 2)
  VsOut[ov_id] = center;

  center.w = 0.0;

  for(i=0; i < fi.len; i++) {
    int id = fi.start+i;
    int v_id = FsIn[id];
    // Add center to all face verts

    //lock(v_id, locks); VsOut[v_id] += center; unlock(v_id, locks);
    // locking doesn't work (for me) do it in a separate pass 
    // single threaded
    
    // Create Faces    
    FsOut[id].x = v_id;
    FsOut[id].y = -5; 
    FsOut[id].z = ov_id; 
    FsOut[id].w = -5;
  }
}

__kernel void add_center(
			 __global int *FsIn,
			 __global FaceIndex *FiIn,
			 __global float4 *VsOut,
			 const uint noFs,
			 const uint noVs
			 )
{
  int i, face_id;
  const int id = get_global_id(0);
  if (id >= 1) return;  // Should only run by one "thread"

  FaceIndex fi;
  uint v_id, ov_id;
  float4 center;
  float4 zero = {0.0,0.0,0.0,0.0}; 

  for(face_id=0; face_id < noFs; face_id++) {
      FaceIndex fi = FiIn[face_id];
      ov_id = noVs + face_id;
      center = VsOut[ov_id];
      center.w = 0.0;
      for(i=0; i < fi.len; i++) {
	  int v_id = FsIn[fi.start+i];	  
	  float4 v = VsOut[v_id];
	  uint he_c = trunc(v.w);
	  he_c = he_c % 4;
	  if(he_c < 2) {
	      VsOut[v_id] = v + center;
	  } else if(he_c == 2) {
	      zero.w = v.w;
	      VsOut[v_id] = zero;
	  };
      }
  }
}

__kernel void gen_edges(__global float4 *VsIn,
			__global int *FsIn,
			__global int4 *EsIn,
			__global FaceIndex *FiIn,
			__global float4 *VsOut,
			__global int  *FsOut,
			__global int4 *EsOut,
			const uint noFs,
			const uint noVs,
			const uint noEs)
{
  int i;
  const int edge_id = get_global_id(0);
  if (edge_id >= noEs)
      return;
  float4 center = {0.0,0.0,0.0,0.0};
  int4 edge = EsIn[edge_id];
  int hard = 0;
  int ov_id = noVs+noFs+edge_id;
  int hov_id = ov_id;
  const int4 hole_edge = {-1,-1,-1,-1};

  if(edge.y < 0) { // Indicates edge in hole
      const int oe_id = edge_id*4;
      EsOut[oe_id+0] = hole_edge;
      EsOut[oe_id+1] = hole_edge;
      EsOut[oe_id+2] = hole_edge;
      EsOut[oe_id+3] = hole_edge;
      return;
  }

  if(edge.x < 0) {  // Indicates hard edge
      hard = 1;
      edge.x = -1-edge.x;
      hov_id = -1-ov_id;
  }

  center += VsIn[edge.x];  // V0
  center += VsIn[edge.y];  // V1
  if(hard) {
      center /= 2.0;
      center.w = 18.0; // Valance 4 and 2 hard edges ((4 << 2) | 2)
  } else {
      center += VsOut[noVs+edge.z]; // F1 Center
      center += VsOut[noVs+edge.w]; // F2 Center
      center /= 4.0;
      center.w = 16.0; // Valance 4 and 0 hard edges ((4 << 2) | 2)
  }
  
  // New vertex at edge center position
  VsOut[ov_id] = center;
  // Complete faces 
  int F11=-1,F12=-1,F21=-1,F22=-1, CCW1,CCW2;
  const int oe_id = edge_id*4;
  // Be sure to create faces with the correct order   
  if(edge.z >= 0) { // Edge is not a border
      FaceIndex IF1 = FiIn[edge.z];
      find_faces(edge.x,edge.y,IF1,FsIn,&F11,&F12,&CCW1);
      const int4 e0 = {ov_id,noVs+edge.z,F11,F12};
      EsOut[oe_id+0] = e0;
      if(CCW1) {
	  FsOut[F11*4+1] = ov_id;
	  FsOut[F12*4+3] = ov_id;
      } else {
	  FsOut[F11*4+3] = ov_id;
	  FsOut[F12*4+1] = ov_id;
      }
  } else {
      EsOut[oe_id+0] = hole_edge;
  }
  if(edge.w >= 0) { // Edge is not a border
      FaceIndex IF2 = FiIn[edge.w];
      find_faces(edge.x,edge.y,IF2,FsIn,&F21,&F22,&CCW2);
      const int4 e1 = {ov_id,noVs+edge.w,F21,F22};
      EsOut[oe_id+1] = e1;
      if(CCW2) {
	  FsOut[F21*4+1] = ov_id;
	  FsOut[F22*4+3] = ov_id;
      } else {
	  FsOut[F21*4+3] = ov_id;
	  FsOut[F22*4+1] = ov_id;
      }
  } else {
      EsOut[oe_id+1] = hole_edge;
  }
  // Hmm init only when declaring var on nvidia? 
  const int4 e2 = {hov_id,edge.x,F11,F21};
  EsOut[oe_id+2] = e2;
  const int4 e3 = {hov_id,edge.y,F12,F22};
  EsOut[oe_id+3] = e3;
}


__kernel void add_edge_verts(
			     __global float4 *VsIn,
			     __global float4 *VsOut,
			     __global int4 *EsIn,
			     const uint noEs
			     )
{
  const int thread = get_global_id(0);
  if (thread >= 1) return;  // Should only run by one "thread"
  
  int id;
  int4 edge;
  float4 v0,v1;
  int hard_v0=0, hard_v1=0;

  for(id=0; id < noEs; id++) {
      edge = EsIn[id];
      if(edge.y >= 0) {
	  if(edge.x < 0) { // Hard edge
	      edge.x = -1-edge.x;
	      v0 = VsIn[edge.x];
	      v0.w = 0.0;
	      VsOut[edge.y] += v0;
	      v1 = VsIn[edge.y];
	      v1.w = 0.0;
	      VsOut[edge.x] += v1;
	  } else { // Only add soft edges if vertex have <2 hardedges
	      v0 = VsIn[edge.x];
	      v1 = VsIn[edge.y];
	      
	      hard_v0 = trunc(v0.w);	  
	      hard_v1 = trunc(v1.w);
	      hard_v0 = hard_v0 % 4;
	      hard_v1 = hard_v1 % 4;
	      if(hard_v1 < 2) {
		  v0.w = 0.0;
		  VsOut[edge.y] += v0;
	      }
	      if(hard_v0 < 2) {
		  v1.w = 0.0;	  
		  VsOut[edge.x] += v1;
	      }
	  }
      }    
  }
}

__kernel void move_verts(
			 __global float4 *VsIn,
			 __global float4 *VsOut,
			 const uint noInVs,
			 const uint noOutVs
			 )
{
  const int v_id = get_global_id(0);
  if(v_id >= noOutVs)
    return;
  if(v_id >= noInVs) {  
    // Copy buffer VsIn and VsOut should be equal
    // after this pass
    VsIn[v_id] = VsOut[v_id];
    return;
  }
  float4 v_in  = VsIn[v_id];
  float4 v_out = VsOut[v_id];
  uint hc = trunc(v_in.w);  
  uint vc = hc; 
  hc = hc % 4;
  vc = vc / 4;
  if(hc < 2) {
    float a = 1.0/(vc*vc);
    float b = (vc-2.0)/vc;
    //  We started with Inpos remove it
    v_out -= v_in;
    v_out *= a;
    v_out += (v_in * b);
    v_out.w = v_in.w;
    VsOut[v_id] = v_out;
    VsIn[v_id] = v_out;
  } else if(hc == 2) {
    v_out += v_in * 6.0;
    v_out *= 1.0/8.0;
    v_out.w = v_in.w;
    VsOut[v_id] = v_out;
    VsIn[v_id] = v_out;
  } else {
    VsOut[v_id] = v_in;
  }
}

__kernel void collect_face_info(
				__global float4 *Vs,
				__global int4 *Fs,
				__global float *Vab,
				const uint noFs
				)
{
    const int id = get_global_id(0);
    if(id >= noFs) 
	return;
    const int f_sz = 4*6;
    int4 face = Fs[id];
    float4 v1, v2, v3, v4, normal;
    v1 = Vs[face.x];
    v2 = Vs[face.y];
    v3 = Vs[face.z];
    v4 = Vs[face.w];
    normal = normalize(cross(v3-v1,v4-v2));
    // Output V1    
    Vab[id*f_sz+0] = v1.x;  Vab[id*f_sz+3] = normal.x;
    Vab[id*f_sz+1] = v1.y;  Vab[id*f_sz+4] = normal.y;
    Vab[id*f_sz+2] = v1.z;  Vab[id*f_sz+5] = normal.z;   
    // Output V2        
    Vab[id*f_sz+6] = v2.x;  Vab[id*f_sz+9]  = normal.x;
    Vab[id*f_sz+7] = v2.y;  Vab[id*f_sz+10] = normal.y;
    Vab[id*f_sz+8] = v2.z;  Vab[id*f_sz+11] = normal.z;
    // Output V3
    Vab[id*f_sz+12] = v3.x; Vab[id*f_sz+15] = normal.x;
    Vab[id*f_sz+13] = v3.y; Vab[id*f_sz+16] = normal.y;
    Vab[id*f_sz+14] = v3.z; Vab[id*f_sz+17] = normal.z;
    // Output V4
    Vab[id*f_sz+18] = v4.x; Vab[id*f_sz+21] = normal.x;
    Vab[id*f_sz+19] = v4.y; Vab[id*f_sz+22] = normal.y;
    Vab[id*f_sz+20] = v4.z; Vab[id*f_sz+23] = normal.z;    
}


// Helpers
// Find the order of faces so that vertices for a face
// comes in the ccw order
void find_faces(int V0, int V1, FaceIndex Fi, __global int *Fs, 
		int * F1, int *F2, int *CCW)
{
    int fva,fvb;
    fva = Fs[Fi.start];
    for(int i=Fi.start; i < (Fi.start+Fi.len); i++) {
	fvb = Fs[i+1];
	if(V0==fva) {
	    *F1 = i;
	    if(V1==fvb) {
		*F2 = i+1;
		*CCW = 1;
	    } else {
		*F2 = i+Fi.len-1;
		*CCW = 0;
	    }
	    return;
	}
	if(V1==fva) {
	    *F2 = i;
	    if(V0==fvb) {
		*F1 = i+1;
		*CCW = 0;
	    } else {
		*F1 = i+Fi.len-1;
		*CCW = 1;
	    }
	    return;
	}
	fva = fvb;
    };
    *F1 = -1;
    *F2 = -2;
    *CCW = 1;
}


// void lock(int v_id, __global int *locks) {
//   int pos = v_id % LOCK_SZ;
//   __global int * semaphor = &(locks[pos]);
//   int occupied = atom_xchg(semaphor, 1);
//   while(occupied > 0) {
//     occupied = atom_xchg(semaphor, 1);
//   }
// }

// void unlock(int v_id, __global int *locks) {
//   int pos = v_id % LOCK_SZ;
//   __global int * semaphor = &(locks[pos]);
//   atom_xchg(semaphor, 0);
// }
