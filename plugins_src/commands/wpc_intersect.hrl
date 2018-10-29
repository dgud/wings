%%
%%  wpc_intersect.hrl --
%%
%%     Header file for wpc_intersection - defines the intersect_data record.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_intersection.erl,??
%%

-include_lib("wings/src/wings.hrl").

% Intersect data
-record(intersect_data,
	{lineDir,
	 linePoint,
	 planeNorm,
	 planePoint,
	 lineDotPlane
	}).
