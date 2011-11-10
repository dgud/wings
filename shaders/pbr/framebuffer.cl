/***************************************************************************
 *   Copyright (C) 1998-2010 by authors (see AUTHORS.txt )                 *
 *                                                                         *
 *   This file is part of LuxRays.                                         *
 *                                                                         *
 *   LuxRays is free software; you can redistribute it and/or modify       *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   LuxRays is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 *                                                                         *
 *   LuxRays website: http://www.luxrender.net                             *
 ***************************************************************************/
// Note: I have appended the files from luxrender to one file /dgud

// NOTE: these kernels assume samples do not overlap

#define FILTER_TABLE_SIZE 16
#define Gaussian2x2_xWidth 2.f
#define Gaussian2x2_yWidth 2.f
#define Gaussian2x2_invXWidth (1.f / Gaussian2x2_xWidth)
#define Gaussian2x2_invYWidth (1.f / Gaussian2x2_invXWidth)

#define GAMMA_TABLE_SIZE 1024u

typedef struct {
	float r, g, b;
} Spectrum;

typedef Spectrum Pixel;

typedef struct {
	Spectrum radiance;
	float weight;
} SamplePixel;

typedef struct {
	float screenX, screenY;
	Spectrum radiance;
} SampleBufferElem;

int Ceil2Int(const float val) {
	return (int)ceil(val);
}

int Floor2Int(const float val) {
	return (int)floor(val);
}

float Clamp(float val, float low, float high) {
	return (val > low) ? ((val < high) ? val : high) : low;
}

unsigned int Floor2UInt(const float val) {
	return (val > 0.f) ? ((unsigned int)floor(val)) : 0;
}

float Radiance2PixelFloat(
		const float x,
		__constant float *gammaTable) {
	//return powf(Clamp(x, 0.f, 1.f), 1.f / 2.2f);

	const unsigned int index = min(
		Floor2UInt(GAMMA_TABLE_SIZE * Clamp(x, 0.f, 1.f)),
			GAMMA_TABLE_SIZE - 1u);
	return gammaTable[index];
}

/*  *************************************** */

void AddSample(__global SamplePixel *sp, const float4 sample) {
    float4 weight = (float4)(sample.w, sample.w, sample.w, 1.f);
    __global float4 *p = (__global float4 *)sp;
    *p += weight * sample;
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(64, 1, 1))) void PixelAddSampleBuffer(
	const unsigned int width,
	const unsigned int height,
	__global SamplePixel *sampleFrameBuffer,
	const unsigned int sampleCount,
	__global SampleBufferElem *sampleBuff) {
	const unsigned int index = get_global_id(0);
	if (index >= sampleCount)
		return;

	__global SampleBufferElem *sampleElem = &sampleBuff[index];
	const unsigned int x = (unsigned int)sampleElem->screenX;
	const unsigned int y = (unsigned int)sampleElem->screenY;
    const float4 sample = (float4)(sampleElem->radiance.r, sampleElem->radiance.g, sampleElem->radiance.b, 1.f);

    AddSample(&sampleFrameBuffer[x + y * width], sample);
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(64, 1, 1))) void PixelAddSampleBufferPreview(
	const unsigned int width,
	const unsigned int height,
	__global SamplePixel *sampleFrameBuffer,
	const unsigned int sampleCount,
	__global SampleBufferElem *sampleBuff) {
	const unsigned int index = get_global_id(0);
	if (index >= sampleCount)
		return;

	const float splatSize = 2.0f;
	__global SampleBufferElem *sampleElem = &sampleBuff[index];

	const float dImageX = sampleElem->screenX - 0.5f;
	const float dImageY = sampleElem->screenY - 0.5f;
    const float4 sample = (float4)(sampleElem->radiance.r, sampleElem->radiance.g, sampleElem->radiance.b, 0.01f);

	int x0 = Ceil2Int(dImageX - splatSize);
	int x1 = Floor2Int(dImageX + splatSize);
	int y0 = Ceil2Int(dImageY - splatSize);
	int y1 = Floor2Int(dImageY + splatSize);

	x0 = max(x0, 0);
	x1 = min(x1, (int)width - 1);
	y0 = max(y0, 0);
	y1 = min(y1, (int)height - 1);

	for (int y = y0; y <= y1; ++y) {
        const unsigned int offset = y * width;

		for (int x = x0; x <= x1; ++x)
            AddSample(&sampleFrameBuffer[offset + x], sample);
	}
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(64, 1, 1))) void PixelAddSampleBufferGaussian2x2(
	const unsigned int width,
	const unsigned int height,
	__global SamplePixel *sampleFrameBuffer,
	const unsigned int sampleCount,
	__global SampleBufferElem *sampleBuff,
    __constant __attribute__((max_constant_size(sizeof(float) * FILTER_TABLE_SIZE * FILTER_TABLE_SIZE))) float *Gaussian2x2_filterTable) {
	const unsigned int index = get_global_id(0);
	if (index >= sampleCount)
		return;

	__global SampleBufferElem *sampleElem = &sampleBuff[index];
	float4 sample = (float4)(sampleElem->radiance.r, sampleElem->radiance.g, sampleElem->radiance.b, 1.f);

	const float dImageX = sampleElem->screenX - 0.5f;
	const float dImageY = sampleElem->screenY - 0.5f;
	const int x0 = Ceil2Int(dImageX - Gaussian2x2_xWidth);
	const int x1 = Floor2Int(dImageX + Gaussian2x2_xWidth);
	const int y0 = Ceil2Int(dImageY - Gaussian2x2_yWidth);
	const int y1 = Floor2Int(dImageY + Gaussian2x2_yWidth);
	if (x1 < x0 || y1 < y0 || x1 < 0 || y1 < 0)
		return;

	// Loop over filter support and add sample to pixel arrays
	__local int ifxBuff[FILTER_TABLE_SIZE * 64];
	__local int *ifx = &(ifxBuff[FILTER_TABLE_SIZE * get_local_id(0)]);
	for (int x = x0; x <= x1; ++x) {
		const float fx = fabs((x - dImageX) *
				Gaussian2x2_invXWidth * FILTER_TABLE_SIZE);
		ifx[x - x0] = min(Floor2Int(fx), (int)FILTER_TABLE_SIZE - 1);
	}

	__local int ifyBuff[FILTER_TABLE_SIZE * 64];
	__local int *ify = &(ifyBuff[FILTER_TABLE_SIZE * get_local_id(0)]);
	for (int y = y0; y <= y1; ++y) {
		const float fy = fabs((y - dImageY) *
				Gaussian2x2_invYWidth * FILTER_TABLE_SIZE);
		ify[y - y0] = min(Floor2Int(fy), (int)FILTER_TABLE_SIZE - 1);
	}

	float filterNorm = 0.f;
	for (int y = y0; y <= y1; ++y) {
		for (int x = x0; x <= x1; ++x) {
			const int offset = ify[y - y0] * FILTER_TABLE_SIZE + ifx[x - x0];
			filterNorm += Gaussian2x2_filterTable[offset];
		}
	}
	filterNorm = 1.f / filterNorm;

	const int fx0 = max(x0, 0);
	const int fx1 = min(x1, (int)width - 1);
	const int fy0 = max(y0, 0);
	const int fy1 = min(y1, (int)height - 1);

	for (int y = fy0; y <= fy1; ++y) {
	    const unsigned int offset = y * width;
	    
	    for (int x = fx0; x <= fx1; ++x) {
		const int tabOffset = ify[y - y0] * FILTER_TABLE_SIZE + ifx[x - x0];
		sample.w = Gaussian2x2_filterTable[tabOffset] * filterNorm;
		
		AddSample(&sampleFrameBuffer[offset + x], sample);
	    }
	}
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(8, 8, 1))) void PixelClearFB(
	const unsigned int width,
	const unsigned int height,
    __global Pixel *frameBuffer) {
    const unsigned int px = get_global_id(0);
    if(px >= width)
        return;
    const unsigned int py = get_global_id(1);
    if(py >= height)
        return;
	const unsigned int offset = px + py * width;

	__global Pixel *p = &frameBuffer[offset];
	p->r = 0.f;
	p->g = 0.f;
	p->b = 0.f;
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(8, 8, 1))) void PixelClearSampleFB(
	const unsigned int width,
	const unsigned int height,
    __global SamplePixel *sampleFrameBuffer) {
    const unsigned int px = get_global_id(0);
    if(px >= width)
        return;
    const unsigned int py = get_global_id(1);
    if(py >= height)
        return;
	const unsigned int offset = px + py * width;

	__global SamplePixel *sp = &sampleFrameBuffer[offset];
	sp->radiance.r = 0.f;
	sp->radiance.g = 0.f;
	sp->radiance.b = 0.f;
	sp->weight = 0.f;
}

/*  *************************************** */

__kernel __attribute__((reqd_work_group_size(8, 8, 1))) void PixelUpdateFrameBuffer(
	const unsigned int width,
	const unsigned int height,
	__global SamplePixel *sampleFrameBuffer,
	__global Pixel *frameBuffer,
	__constant __attribute__((max_constant_size(sizeof(float) * GAMMA_TABLE_SIZE))) float *gammaTable) {
    const unsigned int px = get_global_id(0);
    if(px >= width)
        return;
    const unsigned int py = get_global_id(1);
    if(py >= height)
        return;
	const unsigned int offset = px + py * width;

	__global SamplePixel *sp = &sampleFrameBuffer[offset];
	__global Pixel *p = &frameBuffer[offset];

	const float weight = sp->weight;
	if (weight == 0.f)
		return;

	const float invWeight = 1.f / weight;
	p->r = Radiance2PixelFloat(sp->radiance.r * invWeight, gammaTable);
	p->g = Radiance2PixelFloat(sp->radiance.g * invWeight, gammaTable);
	p->b = Radiance2PixelFloat(sp->radiance.b * invWeight, gammaTable);
}
