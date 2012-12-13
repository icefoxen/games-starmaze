--  Name: MapFunctions v1.52
--  Author: Mikail
--  Created: 10/07/04
--  Last updated: 04/07/08
--  Homepage: http//www.geocities.com/Area51/Quadrant/3864/homeworld.htm
--  Discussion:
--	http://forums.relicnews.com/showthread.php?t=82964
--	http://forums.relicnews.com/showthread.php?t=40779 (old thread)
--  Credits:
--  	MathWorld and Wikipedia for the parametric surface functions in "shapeAdd".


--------------------------------------------------------------------------------
-- Global variables.
--

-- incremental counters
iStartPointsCount = 0
iPointsCount = 0
iSpheresCount = 0
iCamerasCount = 0
iSquadronsCount = 0
iAsteroidsCount = 0
iSalvageCount = 0
iPebblesCount = 0
iCloudsCount = 0
iDustCloudsCount = 0
iNebulaeCount = 0


--------------------------------------------------------------------------------
--  Name:		appendShape
--  Author:		Mikail
--  Description:	General shape-adding function utilized by the other functions.
--  Syntax:		appendShape(<tPos>, <i>, <tPar>, <j>, <tCoo>, <tRot>)
--  Arguments:
--  	<tPos>: a table containing the initial coordinates for the object.
--  	<i>: the index of the object within the distribution table. [obsolete.]
--  	<tPar>: a table containing the object-specific parameters.
--  	<j>: the position number of the object within the distribution table. [obsolete.]
--  	<tCoo>: a table containing the modified coordinates for the object.
--  	<tRot>: a table containing the Euler rotation angles.
--------------------------------------------------------------------------------

function appendShape(tPos, i, tPar, j, tCoo, tRot)
	tCoo = vaddV(vrotate(tCoo, tRot), tPos)
	if (tPar[2] == "Point") then
		local name = tPar[3]
		if (tPar[6] == 1) then
			name = name .. j
		end
		addPoint(name, vaddV(tCoo, tPar[4]), tPar[5])
		iPointsCount = iPointsCount + 1
	elseif (tPar[2] == "StartPoint") then
		local name = "StartPos" .. iStartPointsCount
		addPoint(name, vaddV(tCoo, tPar[4]), tPar[5])
		iStartPointsCount = iStartPointsCount + 1					
	elseif (tPar[2] == "Sphere") then
		local name = tPar[3]
		if (tPar[6] == 1) then
			name = name .. j
		end
		addSphere(name, vaddV(tCoo, tPar[4]), tPar[5])
		iSpheresCount = iSpheresCount + 1
	elseif (tPar[2] == "Camera") then
		local name = tPar[3]
		if (tPar[6] == 1) then
			name = name .. j
		end
		addCamera(name, vaddV(tCoo, tPar[4]), tPar[5])
		iCamerasCount = iCamerasCount + 1
	elseif (tPar[2] == "Squadron") then
		local name = tPar[3]
		if (tPar[10] == 1) then
			name = name .. j
		end
		addSquadron(name, tPar[4], vaddV(tCoo, tPar[5]), tPar[6], tPar[7], tPar[8], tPar[9])
		iSquadronsCount = iSquadronsCount + 1
	elseif (tPar[2] == "Asteroid") then
		addAsteroid(tPar[3], vaddV(tCoo, tPar[4]), tPar[5], tPar[6], tPar[7], tPar[8], tPar[9])
		iAsteroidsCount = iAsteroidsCount + 1
	elseif (tPar[2] == "Salvage") then
		addSalvage(tPar[3], vaddV(tCoo, tPar[4]), tPar[5], tPar[6], tPar[7], tPar[8], tPar[9])
		iSalvageCount = iSalvageCount + 1
	elseif (tPar[2] == "Pebble") then
		addPebble(tPar[3], vaddV(tCoo, tPar[4]), tPar[5], tPar[6], tPar[7])
		iPebblesCount = iPebblesCount + 1
	elseif (tPar[2] == "Cloud") then
		addCloud(tPar[3] .. j, tPar[4], vaddV(tCoo, tPar[5]), tPar[6], tPar[7], tPar[8])
		iCloudsCount = iCloudsCount + 1
	elseif (tPar[2] == "DustCloud") then
		addDustCloud(tPar[3] .. j, tPar[4], vaddV(tCoo, tPar[5]), tPar[6], tPar[7], tPar[8])
		iDustCloudsCount = iDustCloudsCount + 1
	elseif (tPar[2] == "Nebula") then
		addNebula(tPar[3] .. j, tPar[4], vaddV(tCoo, tPar[5]), tPar[6], tPar[7], tPar[8])
		iNebulaeCount = iNebulaeCount + 1
	elseif (tPar[2] == "Coordinate") then
		tinsert(tPar[3], tCoo)
	elseif (tPar[2] == "Function") then
		if (tPar[7] == nil) then
			tPar[7] = {0,0,0,}
		end
		tPar[3](vaddV(tCoo, tPar[4]), tPar[5], tPar[6], vaddV(tRot, tPar[7]))
	end
end

function addCoordinate(tCoo, rTable)
	tinsert(rTable, tCoo)
end

function addSOBGroup(sSobName, ...)
	createSOBGroup(sSobName)
	for i, iCount in arg do
		addToSOBGroup(iCount, sSobName)
	end
end


--------------------------------------------------------------------------------
--  Name:		branchAdd
--  Author:		Mikail
--  Description:	Creates a branching tree.
--  Syntax:		branchAdd(<tPos>, <tDst>, {<tDiv>, <tInt>, <tFrq>, <tBeg>, <tEnd>, <tRad>, <tLen>, <tThk>, <tAng>, <iMod>,}, <tRot>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tDst>: the distribution table used to populate the shape.
--	<tPar>: a table containing the following ten parameters:
--		<tDiv>: a table containing the minimum and maximum number of new shoots that are generated each time the tree divides.
--		<tInt>: a table containing the minimum and maximum number of segments between instances of division.
--		<tFrq>: a table containing the minimum and maximum number of times the tree divides.
--		<tBeg>: a table containing the minimum and maximum number of segments added to the beginning of the tree.
--		<tEnd>: a table containing the minimum and maximum number of segments added to the end of the tree (at the end of each branch). [Note: needs to be greater than zero for the last division to be noticable.]
--		<tRad>: a table containing the the minimum and maximum radius of a segment.
--		<tLen>: a table containing the minimum and maximum length of a segment.
--		<tThk>: a table containing the minimum and maximum thickness, as percentages of the radius, of a segment.
--		<tAng>: a table containing the minimum and maximum angle of deviation between segments.
--	<iMod>: 0 is non-random mode, 1 is random placement with steadily decreasing length and radius, 2 is random placement with random length and radius, 3 is a bug-ridden method using splines.
--	<fRotation>: a table containing the X, Y and Z rotation angles (degrees) for the entire object.
--	The remaining arguments are used internally by the function.
--------------------------------------------------------------------------------

function branchAdd(tPos, tDst, tPar, tRot, lastRad, countDiv, countFrq, lastPos)
	local tCoo = {}
	local tDiv, tInt, tFrq, tBeg, tEnd, tRad, tLen, tThk, tAng, iMod = tPar[1], tPar[2], tPar[3], tPar[4], tPar[5], tPar[6], tPar[7], tPar[8], tPar[9], tPar[10]
	local thisPos = {}
	local nextPos = tPos
	local tiltDeg, spinDeg = tRot[3], tRot[2]
	local minRad, maxRad = tRad[1], tRad[2]
	local minDist, maxDist = tLen[1], tLen[2]
	local minThck, maxThck = 1 - tThk[1] / 100, 1 - tThk[2] / 100
	local minAng, maxAng = tAng[1], tAng[2]
	local minBeg, maxBeg = tBeg[1], tBeg[2]
	local minEnd, maxEnd = tEnd[1], tEnd[2]
	local minDiv, maxDiv = tDiv[1], tDiv[2]
	local minInt, maxInt = tInt[1], tInt[2]
	local minFrq, maxFrq = tFrq[1], tFrq[2]
	local numSeg, numInt, numDiv, numBeg, numEnd, numFrq = 0, 0, 0, 0, 0, 0
	if (lastRad == nil) then
		lastRad = maxRad
	end
	if (countDiv == nil) then
		countDiv = 1
	end
	if (countFrq == nil) then
		countFrq = maxFrq
	else
		countFrq = countFrq - 1
	end
	if (lastPos == nil) then
		lastPos = tPos
	end
	local thisRad = sqrt(lastRad^2 / countDiv)
	-- if set to random mode, or random mode with decreasing length and radius
	if ((iMod == 3) or (iMod == 2) or (iMod == 1)) then
		numInt = random2(minInt, maxInt)
		numDiv = random2(minDiv, maxDiv)
		numBeg = random2(minBeg, maxBeg)
		numEnd = random2(minEnd, maxEnd)
		numFrq = random2(minFrq, maxFrq)
	-- if set to non-random mode
	elseif (iMod == 0) then
		numInt = maxInt
		numDiv = minDiv
		numBeg = maxBeg
		numEnd = maxEnd
		numFrq = maxFrq
	end
	if (numBeg > 0) then
		numSeg = numBeg
	elseif (numFrq > 0) then
		numSeg = numInt
	elseif (numEnd > 0) then
		numSeg = numEnd
	end
	for k = 1, numSeg do
		local rad, len, thk, angY, angZ = 0, 0, 0, 0, 0
		-- if set to non-random mode
		if (iMod == 0) then
			local sign1, sign2 = randomSign(), randomSign()
			rad, len, angY, angZ = maxRad, maxDist, sign1 * maxAng, sign2 * maxAng
		-- if set to random mode with decreasing length and radius
		elseif (iMod == 1) then
			local sign1, sign2 = randomSign(), randomSign()
			local narrw = random3(0.9, 1)
			thisRad = thisRad * narrw
			thk, rad, len, angY, angZ = random3(minThck, maxThck) * narrw, thisRad, maxDist - (maxDist - minDist) / (numFrq + 1), random3(minAng, maxAng) * sign1, random3(minAng, maxAng) * sign2
		-- if set to random mode
		elseif (iMod == 2) then
			local sign1, sign2 = randomSign(), randomSign()
			thk, rad, len, angY, angZ = random3(minThck, maxThck), random3(minRad, maxRad), random3(minDist, maxDist), random3(minAng, maxAng) * sign1, random3(minAng, maxAng) * sign2
		end
		tiltDeg = tiltDeg + angZ
		spinDeg = spinDeg + angY
		thisPos = nextPos
		nextPos = vaddV(thisPos, vrotate({len, 0, 0,}, {0, spinDeg, tiltDeg,}))
		for i, tTab in tDst do
			local gradX = len
			local Volume1, Volume2 = PI * maxRad^2 * maxDist, PI * len * (rad^2 - (rad * thk)^2)
			local Density = Volume2 / Volume1
			local iNum = floor(tTab[1] * Density + 0.5)
			for j = 1, iNum do
				if (iMod == 0) then
					tCoo = {gradX, 0, 0,}
					appendShape(thisPos, i, tTab, j, tCoo, {tRot[1], spinDeg, tiltDeg,})
				-- random method using splines. doesn't work yet because four points need to be known, instead of just three
				elseif (iMod == 3) then
					local t = random()
					local thisCtl = vaddV(thisPos, vsubtractV(thisPos, lastPos))
					local nextCtl = vsubtractV(nextPos, vsubtractV(nextPos, thisPos))
					local A1_x, A1_y, A1_z = thisPos[1], thisPos[2], thisPos[3]
					local A2_x, A2_y, A2_z = thisCtl[1], thisCtl[2], thisCtl[3]
					local B1_x, B1_y, B1_z = nextPos[1], nextPos[2], nextPos[3]
					local B2_x, B2_y, B2_z = nextCtl[1], nextCtl[2], nextCtl[3]
					tCoo[1] = (B1_x + 3 * A2_x - 3 * B2_x - A1_x) * t^3 + (3 * B2_x - 6 * A2_x + 3 * A1_x) * t^2 + (3 * A2_x - 3 * A1_x) * t + A1_x 
					tCoo[2] = (B1_y + 3 * A2_y - 3 * B2_y - A1_y) * t^3 + (3 * B2_y - 6 * A2_y + 3 * A1_y) * t^2 + (3 * A2_y - 3 * A1_y) * t + A1_y 
					tCoo[3] = (B1_z + 3 * A2_z - 3 * B2_z - A1_z) * t^3 + (3 * B2_z - 6 * A2_z + 3 * A1_z) * t^2 + (3 * A2_z - 3 * A1_z) * t + A1_z 
					appendShape(thisPos, i, tTab, j, tCoo, tRot)
				elseif (iMod == 2) then
					local r, v, h = random3(rad * thk, rad), random3(360), random3(len)
					tCoo = {h, r * cos(v), r * sin(v),}
					appendShape(thisPos, i, tTab, j, tCoo, {tRot[1], spinDeg, tiltDeg,})
				elseif (iMod == 1) then
					local r, v, h = random3(rad * thk, rad), random3(360), random3(len)
					tCoo = {h, r * cos(v), r * sin(v),}
					appendShape(thisPos, i, tTab, j, tCoo, {tRot[1], spinDeg, tiltDeg,})
				end
				gradX = gradX - len / iNum
			end
		end
		lastPos = thisPos
	end
	if (numBeg > 0) then
		tBeg = {0, 0,}
		branchAdd(nextPos, tDst, {tDiv, tInt, tFrq, tBeg, tEnd, tRad, tLen, tThk, tAng, iMod,}, tRot, nil, nil, nil, thisPos)
	elseif (numFrq > 0) then
		if (minFrq >= numFrq) then
			minFrq = numFrq - 1
		end
		tFrq = {minFrq, numFrq - 1,}
		for j = 1, numDiv do
			branchAdd(nextPos, tDst, {tDiv, tInt, tFrq, tBeg, tEnd, tRad, tLen, tThk, tAng, iMod,}, tRot, thisRad, numDiv, countFrq, thisPos)
		end
	elseif (numEnd > 0) then
		tDiv, tInt, tFrq, tEnd = {0, 0,}, {0, 0,}, {0, 0,}, {0, 0,}
		branchAdd(nextPos, tDst, {tDiv, tInt, tFrq, tBeg, tEnd, tRad, tLen, tThk, tAng, iMod,}, tRot, thisRad, numDiv, countFrq, thisPos)
	end
end


--------------------------------------------------------------------------------
--  Name:		splineAdd
--  Author:		Mikail
--  Description:	Creates a spline-shaped tube connecting any two points using two control points.
--  Syntax:		splineAdd(<tPos>, <tDst>, {<tP1A>, <tP1B>, <tP2A>, <tP2B>, <tRad>,}, <tRot>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tDst>: the distribution table used to populate the shape.
--	<tPar>: a table containing the following six parameters:
--		<tP1A>: a table containing the coordinates of the starting point.
--		<tP1B>: a table containing the coordinates of the first control point.
--		<tP2A>: a table containing the coordinates of the ending point.
--		<tP2B>: a table containing the coordinates of the second control point.
--		<tRad>: a table containing the initial and final radii of the tube.
--		<tThk>: a table containing the minimum and maximum thickness, as percentages of the radius, of the tube.
--	<tRot>: a table containing the X, Y and Z rotation angles (degrees) for the entire object.
--------------------------------------------------------------------------------

function splineAdd(tPos, tDst, tPar, tRot)
	local tCoo = {}
	local tDerivatives = {}
	local tP1A, tP1B, tP2A, tP2B, tRad, tThk = tPar[1], tPar[2], tPar[3], tPar[4], tPar[5], tPar[6]
	local X, Y, Z = 0, 0, 0
	local minRad, maxRad, minThk, maxThk = tRad[1], tRad[2], 1 - tThk[1] / 100, 1 - tThk[2] / 100
	local A1_x, A1_y, A1_z = tP1A[1], tP1A[2], tP1A[3]
	local A2_x, A2_y, A2_z = tP1B[1], tP1B[2], tP1B[3]
	local B1_x, B1_y, B1_z = tP2A[1], tP2A[2], tP2A[3]
	local B2_x, B2_y, B2_z = tP2B[1], tP2B[2], tP2B[3]
	for i, tTab in tDst do
		local iNum = tTab[1]
		for j = 1, iNum do
			local phi, r, t = 0, 0, 0
			t = random()
			phi = random(360)
			r = (minRad + (maxRad - minRad) * t) * random3(minThk, maxThk)
			X = r * cos(phi)
			Y = r * sin(phi)
			Z = 0
			tCoo[1] = (B1_x + 3 * A2_x - 3 * B2_x - A1_x) * t^3 + (3 * B2_x - 6 * A2_x + 3 * A1_x) * t^2 + (3 * A2_x - 3 * A1_x) * t + A1_x
			tCoo[2] = (B1_y + 3 * A2_y - 3 * B2_y - A1_y) * t^3 + (3 * B2_y - 6 * A2_y + 3 * A1_y) * t^2 + (3 * A2_y - 3 * A1_y) * t + A1_y
			tCoo[3] = (B1_z + 3 * A2_z - 3 * B2_z - A1_z) * t^3 + (3 * B2_z - 6 * A2_z + 3 * A1_z) * t^2 + (3 * A2_z - 3 * A1_z) * t + A1_z
			tDerivatives[1] = (B1_x + 3 * A2_x - 3 * B2_x - A1_x) * 3 * t^2 + (3 * B2_x - 6 * A2_x + 3 * A1_x) * 2 * t + (3 * A2_x - 3 * A1_x)
			tDerivatives[2] = (B1_y + 3 * A2_y - 3 * B2_y - A1_y) * 3 * t^2 + (3 * B2_y - 6 * A2_y + 3 * A1_y) * 2 * t + (3 * A2_y - 3 * A1_y)
			tDerivatives[3] = (B1_z + 3 * A2_z - 3 * B2_z - A1_z) * 3 * t^2 + (3 * B2_z - 6 * A2_z + 3 * A1_z) * 2 * t + (3 * A2_z - 3 * A1_z)
			local tRotAng = vanglesXY(tDerivatives)
			local tNormedPos = vrotate({X, Y, Z,}, vmultiply(tRotAng, -1))
--			addAsteroid("Asteroid_1", vTangent, 100, 0, 0, 0, 0)
			tCoo = vaddV(tCoo, tNormedPos)
			appendShape(tPos, i, tTab, j, tCoo, tRot)
		end
	end
end


--------------------------------------------------------------------------------
--  Name:		ringAdd
--  Author:		Mikail
--  Description:	Creates an elliptical ring.
--  Syntax:		ringAdd(<tPos>, <tDst>, {<fAx1>, <fAx2>, <fThk>, <fHgh>, <tArc>, <iMod>,}, <tRot>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tDst>: the distribution table used to populate the shape.
--	<tPar>: a table containing the following six parameters:
--		<fAx1>: the length of axis 1.
--		<fAx2>: the length of axis 2.
--		<fThk>: the distance from the outer radius to the inner radius (varies according to <iMod>).
--		<fHgh>: the height of the ring, relative to the plane.
--		<tArc>: a table containing the beginning and ending degrees of the arc.
--		<iMod>: if 0, then non-random mode. If 1, then random mode w/ gradual width. If 2, then random mode w/ even width.
--	<tRot>: a table containing the X, Y and Z rotation angles (degrees) for the entire object.
--------------------------------------------------------------------------------

function ringAdd(tPos, tDst, tPar, tRot)
	local tCoo = {}
	local X, Y, Z = 0, 0, 0
	local fAx1, fAx2, fThk, fHgh, tArc, iMod = tPar[1], tPar[2], tPar[3], tPar[4], tPar[5], tPar[6]
	local minArc, maxArc = tArc[1], tArc[2]
	for i, tTab in tDst do
		local u, w, h, arc = 0, 0, 0, 0
		local iNum = tTab[1]
		for j = 1, iNum do
			-- if set to random mode w/ even width
			if (iMod == 2) then
				u = random3(minArc, maxArc)
				X = cos(u) * fAx2 - random3(fThk)
				Y = random3(fHgh) - fHgh / 2
				Z = sin(u) * fAx1 - random3(fThk)
			-- if set to random mode w/ gradual width
			elseif (iMod == 1) then
				u = random3(minArc, maxArc)
				X = cos(u) * (fAx2 - random3(fThk))
				Y = random3(fHgh) - fHgh / 2
				Z = sin(u) * sqrt((fAx2 - random3(fThk))^2 - fAx2^2 + fAx1^2)
			-- if set to non-random mode
			elseif (iMod == 0) then
				u = minArc + arc
				X = cos(u) * (fAx2 + fThk / -2 + w)
				Y = fHgh / -2 + h
				Z = sin(u) * (fAx1 + fThk / -2 + w)
				arc = arc + (maxArc - minArc) / iNum
				w = w + fThk / iNum
				h = h + fHgh / iNum
			end
			tCoo = {X, Y, Z,}
			appendShape(tPos, i, tTab, j, tCoo, tRot)
		end
	end
end


--------------------------------------------------------------------------------
--  Name:		globeAdd
--  Author:		Mikail
--  Description:	Creates a series of rings in the shape of a sphere, like the latitudinal and longitudinal lines of a globe.
--  Syntax:		globeAdd(<tPos>, <tDst>, {<fRad>, <fLat>, <fLon>, <fThk>, <fHgh>, <tArc>, <iMod>,}, <xNull>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tPar>: a table containing the following seven parameters:
--		<fRad>: the radius of the sphere.
--		<fLat>: the number of latitudinal rings.
--		<fLon>: the number of longitudinal rings.
--		<fThk>: see the description for the "ringAdd" function.
--		<fHgh>: see the description for the "ringAdd" function.
--		<tArc>: see the description for the "ringAdd" function.
--		<iMod>: see the description for the "ringAdd" function.
--	<xNull>: this argument is ignored and is used only to adhere to function call standards.
--  Special note:
--  	Beware that objects may overlap where the longitudinal rings intersect at the poles.
--------------------------------------------------------------------------------

function globeAdd(tPos, tDst, tPar, xNull)
	local lat, lon, u, X, Y = 0, 0, 0, 0, 0
	local fRad, fLat, fLon, fThk, fHgh, tArc, iMod = tPar[1], tPar[2], tPar[3], tPar[4], tPar[5], tPar[6], tPar[7]
	for i = 1, fLat do
		lat = lat + 360 / (fLat * 2 + 2)
		u = lat
		X = cos(u) * fRad
		Y = sin(u) * fRad
		-- ringAdd:	tPos, tDst, tAxi, fThk, fHgh, tArc, tRot, iMod
		ringAdd(vaddV(tPos, {0, X, 0,}), tDst, {Y, Y, fThk, fHgh, tArc,}, {0, 0, 0,}, iMod)
	end
	for i = 1, fLon do
		lon = lon + 360 / fLon
		-- ringAdd:	tPos, tDst, tAxi, fThk, fHgh, tArc, tRot, iMod
		ringAdd(tPos, tDst, {fRad, fRad, fThk, fHgh, tArc,}, {0, lon, 90,}, iMod)
	end
end


--------------------------------------------------------------------------------
--  Name:		shapeAdd
--  Author:		Mikail
--  Description:	Creates one of several available shapes.
--  Syntax:		shapeAdd(<tPos>, <tDst>, {<sLay>, <fA>, <fB>, ...}, <tRot>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tDst>: the distribution table used to populate the shape.
--	<tPar>: a table containing the following six parameters: <fA>, <fB>, <fC>, <fD> and <fE> vary depending on <sLay>.
--		<sLay>: the type of shape to generate.
--		If <sLay> is "Cylinder", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Cone", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Ellipsoid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Cuboid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Toroid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the width of the tube, <fD> equals the thickness, and <fE> equals the height of the tube.
--		If <sLay> is "Helicoid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, and <fD> equals the width (thickness is not supported), <fE> is the number of revolutions.
--		If <sLay> is "Paraboloid", then <fA> equals the length of axis 1 at a height of 1000 units, <fB> equals the length of axis 2 at a height of 1000 units, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Hyperboloid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Astroid", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Funnel", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals roughly the inverse of the length of axis 3 for large numbers of objects, <fD> equals the thickness, and <fE> is zero.
--		If <sLay> is "Dini", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the distance between each twist, <fD> is zero, and <fE> is the number of twists.
--		If <sLay> is "Corkscrew", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the height of the screw, <fD> is zero, and <fE> is zero.
--		If <sLay> is "Seashell", then <fA> equals the length of axis 1 of the tube, <fB> equals the length of axis 2 of the tube, <fC> equals the vertical separation between revolutions, <fD> equals the radius of the center gap, and <fE> equals the number of revolutions.
--		If <sLay> is "SineDisc", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the maximum height of the wave, <fD> zero, and <fE> equals the frequency of the wave pattern.
--		If <sLay> is "SinePlane", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the maximum height of the wave, <fD> zero, and <fE> equals the frequency of the wave pattern.
--		If <sLay> is "Moebius", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals half the width of the strip, <fD> is zero, and <fE> is zero.
--		If <sLay> is "Klein", then <fA> equals the scaling along the x-axis, <fB> equals the scaling along the z-axis, <fC> equals the scaling along the y-axis, <fD> is zero, and <fE> is zero.
--		If <sLay> is "Klein8", then <fA> equals the scaling along the x-axis, <fB> equals the scaling along the z-axis, <fC> equals the scaling along the y-axis, <fD> is zero, and <fE> is zero.
--		If <sLay> is "Boy", then <fA> equals the scaling along the x-axis, <fB> equals the scaling along the z-axis, <fC> equals the scaling along the y-axis, <fD> is zero, and <fE> is an integer greater than 2.
--		If <sLay> is "Lissajous3D", then <fA> equals the length of the bounding cube, <fB> equals the radius of the sphere sweep, <fD> equals the speed of the second arc relative to the first, <fD> equals the speed of the third arc relative to the first, and <fE> equals the number of revolutions.
--		If <sLay> is "Rectangle", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, and <fD> equals the thickness.
--		If <sLay> is "Ellipse", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the length of axis 3, and <fD> equals the thickness.
--		If <sLay> is "Triangle", [...to do.]
--		If <sLay> is "Parabola", then <fA> equals the distance between the vertex and the focus, <fB> equals the length, <fC> equals the height, and <fD> equals the thickness.
--		If <sLay> is "Hyperbola", then <fA> equals the length of axis 1, <fB> equals the length of axis 2, <fC> equals the distance from the origin to one of the foci, <fD> equals the thickness.
--		If <sLay> is "Catenary", [...to do.]
--		If <sLay> is "Hypotrochoid", then <fA> equals the radius of the greater circle, <fB> equals the radius of the lesser circle, <fC> equals the radius of the sphere sweep, <fD> equals the distance from the center of the lesser circle, and <fE> equals the number of revolutions.
--		If <sLay> is "Epitrochoid", then <fA> equals the radius of the greater circle, <fB> equals the radius of the lesser circle, <fC> equals the radius of the sphere sweep, <fD> equals the distance from the center of the lesser circle, and <fE> equals the number of revolutions.
--		If <sLay> is "Lissajous2D", then <fA> equals the length of the bounding square, <fB> equals the phase shift, <fC> equals the radius of the sphere sweep, <fD> equals the speed of the second arc relative to the first, and <fE> equals the number of revolutions.
--	<tRot>: a table containing the X, Y and Z Euler rotation angles, in degrees, for the entire object.
--------------------------------------------------------------------------------

function shapeAdd(tPos, tDst, tPar, tRot)
	local sLay = tPar[1]
	for i, tTab in tDst do
		local iNum = tTab[1]
		for j = 1, iNum do
			local tCoo = {}
			if (sLay == "Cuboid") then
				tCoo = makeCuboid(tPar)
			elseif (sLay == "Ellipsoid") then
				tCoo = makeEllipsoid(tPar)
			elseif (sLay == "Cylinder") then
				tCoo = makeCylinder(tPar)
			elseif (sLay == "Cone") then
				tCoo = makeCone(tPar)
			elseif (sLay == "Toroid") then
				tCoo = makeToroid(tPar)
			elseif (sLay == "Helicoid") then
				tCoo = makeHelicoid(tPar)
			elseif (sLay == "Paraboloid") then
				tCoo = makeParaboloid(tPar)
			elseif (sLay == "Hyperboloid") then
				tCoo = makeHyperboloid(tPar)
			elseif (sLay == "Astroid") then
				tCoo = makeAstroid(tPar)
			elseif (sLay == "Funnel") then
				tCoo = makeFunnel(tPar)
			elseif (sLay == "Dini") then
				tCoo = makeDini(tPar)
			elseif (sLay == "Corkscrew") then
				tCoo = makeCorkscrew(tPar)
			elseif (sLay == "Seashell") then
				tCoo = makeSeashell(tPar)
			elseif (sLay == "SineDisc") then
				tCoo = makeSineDisc(tPar)
			elseif (sLay == "SinePlane") then
				tCoo = makeSinePlane(tPar)
			elseif (sLay == "Moebius") then
				tCoo = makeMoebius(tPar)
			elseif (sLay == "Klein") then
				tCoo = makeKlein(tPar)
			elseif (sLay == "Klein8") then
				tCoo = makeKlein8(tPar)
			elseif (sLay == "Kuen") then
				tCoo = makeKuen(tPar)
			elseif (sLay == "Boy") then
				tCoo = makeBoy(tPar)
			elseif (sLay == "Lissajous3D") then
				tCoo = makeLissajous3D(tPar)
			elseif (sLay == "Rectangle") then
				tCoo = makeRectangle(tPar)
			elseif (sLay == "Ellipse") then
				tCoo = makeEllipse(tPar)
			elseif (sLay == "Triangle") then
				-- to do
			elseif (sLay == "Parabola") then
				tCoo = makeParabola(tPar)
			elseif (sLay == "Hyperbola") then
				tCoo = makeHyperbola(tPar)
			elseif (sLay == "Catenary") then
				-- to do
			elseif (sLay == "Hypotrochoid") then
				tCoo = makeHypotrochoid(tPar)
			elseif (sLay == "Epitrochoid") then
				tCoo = makeEpitrochoid(tPar)
			elseif (sLay == "Lissajous2D") then
				tCoo = makeLissajous2D(tPar)
			end
			appendShape(tPos, i, tTab, j, tCoo, tRot)
		end
	end
end

--------------------------------------------------------------------------------
--  Name:		spiralAdd
--  Author:		Mikail
--  Description:	Creates a spiral.
--  Suntax:		spiralAdd(<tPos>, <tDst>, {<sLay>, <nRad>, <nArm>, <nRot>, <nAng>, <nHgh>, <nWid>, <nThk>, <tTim>, <iMod>,}, <tRot>)
--  Arguments:
--	<tPos>: a table containing the shape's center coordinates.
--	<tDst>: the distribution table used to populate the shape.
--	<tPar>: a table containing the following ten parameters:
--		<sLay>: may be either "Nautilus" or "Archimedes".
--		<fRad>: depending on <fAng>, this is either the minimum or maximum radius of the spiral.
--		<iArm>: the number of arms the spiral will have.
--		<fRot>: the number of times the spiral will rotate around the origin.
--		<fAng>: the angle (degrees) of deviation (90' and 270' makes a circle).
--		<fHgh>: the height of the spiral above the plane.
--		<fWid>: the width of the spiral arms.
--		<fThk>: the thickness of the spiral arms.
--		<tTim>: a table containing the minimum and maximum values for "t" (time, percent) at which the curve is sampled. (must be a float between 0 and 1)
--		<iMod>: 0 is non-random, 1 is random, 2 is random-mode with tapering width.
--	<tRot>: a table containing the X, Y and Z rotation angles (degrees) for the entire object.
--------------------------------------------------------------------------------

function spiralAdd(tPos, tDst, tPar, tRot)
	local tCoo = {}
	local sLay, nRad, nArm, nRot, nAng, nHgh, nWid, nThk, tTim, iMod = tPar[1], tPar[2], tPar[3], tPar[4], tPar[5], tPar[6], tPar[7], tPar[8], tPar[9], tPar[10]
	local minTim, maxTim = tTim[1], tTim[2]
	local X, Y, Z = 0, 0, 0
	local v, t, s = 360 * nRot, 0, 0
	local l, w, h = 0, 0, 0
	for i, tTab in tDst do
		local t_X = minTim
		local l_X, w_X, h_X = nWid / -2, nWid / -2, nThk / -2
		local rotArm = 0
		local iNum = tTab[1]
		for j = 1, iNum do
			-- if set to random mode w/ even width
			if (iMod == 1) then
				v, t, s = v, random3(minTim, maxTim), t
				if (sLay == "Archimedes") then
				--	s = 1 - t
				end
				l, w, h = random3(nWid) - nWid / 2, random3(nWid) - nWid / 2, random3(nThk) - nThk / 2
			-- if set to random mode tapering width
			elseif (iMod == 2) then
				v, t, s = v, random3(minTim, maxTim), t
				if (sLay == "Archimedes") then
				--	s = 1 - t
				end
				l, w, h = random3(0, nWid * s), random3(0, nWid * s), random3(nThk) - nThk / 2
			-- if set to non-random mode
			elseif (iMod == 0) then
				v, t = v, t_X
				l, w, h = l_X, w_X, h_X
				t_X = t_X + (maxTim - minTim) / iNum
				l_X, w_X, h_X = l_X + nWid / iNum, w_X + nWid / iNum, h_X + nThk / iNum
			end
			if (sLay == "Nautilus") then
				-- exp(rad(v * t) / tan(nAng)) is the starter value
				X = exp(rad(v * t) / tan(nAng)) * cos(v * t) * nRad + l
				Y = h - nHgh * t + nHgh / 2
				Z = exp(rad(v * t) / tan(nAng)) * sin(v * t) * nRad + w
			elseif (sLay == "Archimedes") then
				X = cos(v * t) * t * nRad + l
				Y = h + nHgh * t - nHgh / 2
				Z = sin(v * t) * t * nRad + w
			end
			for h = 1, nArm do
				tCoo = vrotate({X, Y, Z,}, {0, rotArm, 0,})
				appendShape(tPos, i, tTab, j, tCoo, tRot)
				rotArm = rotArm + (360 / nArm)
			end
		end
	end
end


--------------------------------------------------------------------------------
--  Name:		literalAdd
--  Author:		Mikail
--  Description:	Adds the contents of a distribution table to the map without changing any values.
--  Syntax:		literalAdd(<tDst>)
--  Arguments:
--	<tDst>: the distribution table used to populate the shape.
--------------------------------------------------------------------------------

function literalAdd(tDst)
	for i, tTab in tDst do
		local iNum = tTab[1]
		for j = 1, iNum do
			appendShape({0, 0, 0,}, i, tTab, j, {0, 0, 0,}, {0, 0, 0,})
		end
	end
end


--------------------------------------------------------------------------------
--  Name:		easyPatch
--  Author:		Mikail
--  Description:	Creates a resource patch with minimal effort.
--  Syntax:		easyPatch(<tPos>, <fRUs>)
--  Arguments:
--	<tPos>: the shape's center coordinates.
--	<fRUs>: (optional) the percent of the default RU to retain.
--------------------------------------------------------------------------------

function easyPatch(tPos, fRUs)
	local tCoo = {}
	local easyPatchDist =
	{
		-- number of asteroids, inner radius, outer radius
		Asteroid_4 = {1, 0, 0,},
		Asteroid_3 = {3, 400, 800,},
		Asteroid_2 = {5, 800, 1600,},
	}
	local AstVal = 100
	if (fRUs) then
		AstVal = fRUs
	end
	for k, tTab in easyPatchDist do
		for j = 1, tTab[1] do
			local r, v, u = random3(tTab[2], tTab[3]), random3(180), random3(360)
			tCoo[1] = tPos[1] + sqrt(r^2 - (r * cos(v))^2) * cos(u)
			tCoo[2] = tPos[2] + r * cos(v)
			tCoo[3] = tPos[3] + sqrt(r^2 - (r * cos(v))^2) * sin(u)
			addAsteroid(k, tCoo, AstVal, 0, 0, 0, 0)
		end
	end
end


--------------------------------------------------------------------------------
--  Name:		randomMusic
--  Author:		Mikail
--  Description:	Randomly selects the level music.
--  Syntax:		randomMusic(<iMod>, <tTab>, <iLen>, <sDir>)
--  Arguments:
--	<iMod>: may be 0, 1, 2, 3, 4 or 5.
--	If <iMod> is 0, then this function is disabled.
--	If <iMod> is 1, then the level music is selected from only the ambient tracks.
--	If <iMod> is 2, then the level music is selected from only the battle tracks.
--	If <iMod> is 3, then the level music is selected from both the ambient and battle tracks.
--	If <iMod> is 4, then the level music is selected from only <tTab>.
--	If <iMod> is 5, then the level music is selected from all of the above.
--	<tTab>: (optional) a table containing the names of the custom audio tracks. (table)
--	<iLen>: (optional) the length of <tTab>. (number)
--	<sDir>: (optional) the directory where the extra files can be found. Must have a trailing slash. (string)
--  Note:
--	This function must be called from within the "NonDetermChunk"" portion of a ".level" file.
--------------------------------------------------------------------------------

function randomMusic(iMod, tTab, iLen, sDir)
	local ranNum = 0
	local musDir = ""
	local musTrk = ""
	local musTab =
	{
		"amb_01", "amb_02", "amb_03", "amb_04", "amb_05", "amb_06", "amb_07", "amb_08", "amb_09", "amb_10", "amb_11", "amb_12", "amb_13", "amb_14",
		"battle_01", "battle_04", "battle_04_alt", "battle_06", "battle_keeper", "battle_movers", "battle_planetkillers", "battle_sajuuk", "bentus_arrival",
	}
	if ((iMod == 4) or (iMod == 5)) then
		for k = 1, iLen do
			musTab[k + 23] = tTab[k]
		end
	end
	if (iMod == 1) then
		ranNum = random(1, 14)
	elseif (iMod == 2) then
		ranNum = random(15, 23)
	elseif (iMod == 3) then
		ranNum = random(1, 23)
	elseif (iMod == 4) then
		ranNum = random(24, 23 + iLen)
	elseif (iMod == 5) then
		ranNum = random(1, 23 + iLen)
	end
	if (ranNum <= 14) then
		musDir = "data:sound\\music\\ambient\\"
	elseif (ranNum <= 23) then
		musDir = "data:sound\\music\\battle\\"
	elseif (ranNum <= (23 + iLen)) then
		musDir = sDir
	end
	if (iMod ~= 0) then
		local musTrk = musDir .. musTab[ranNum]
		setDefaultMusic(musTrk)
		print("Level music: \"" .. musTrk .. "\"")
	end
end


--------------------------------------------------------------------------------
--  Name:		randomBackground
--  Author:		Mikail
--  Description:	Randomly selects the level background.
--  Syntax:		randomBackground(<iMod>, <tTab>, <iLen>)
--  Arguments:
--	<iMod>: may be 0, 1, 2, 3, 4 or 5.
--	If <iMod> is 0, then this function has been disabled.
--	If <iMod> is 1, then the level background is selected from only the mission backgrounds.
--	If <iMod> is 2, then the level background is selected from only the other backgrounds.
--	If <iMod> is 3, then the level background is selected from both the mission and other backgrounds.
--	If <iMod> is 4, then the level background is selected from only <tTab>.
--	If <iMod> is 5, then the level background is selected from all of the above.
--	<tTab>: (optional) a table containing the names of the custom backgrounds.
--	<iLen>: (optional) the length of <tTab>.
--  Note:
--	This function must be called from within the "NonDetermChunk"" portion of a ".level" file.
--------------------------------------------------------------------------------

function randomBackground(iMod, tTab, iLen)
	local ranNum = 0
	local backgroundTable =
	{
		"m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10", "m11", "m12", "m13", "m14", "m15",
		"planet", "quick", "singlesun", "tanis", "taniswstars", "black", "white",
	}
	if ((iMod == 4) or (iMod == 5)) then
		for k = 1, iLen do
			backgroundTable[k + 22] = tTab[k]
		end
	end
	if (iMod == 1) then
		ranNum = random(1, 15)
	elseif (iMod == 2) then
		ranNum = random(16, 22)
	elseif (iMod == 3) then
		ranNum = random(1, 22)
	elseif (iMod == 4) then
		ranNum = random(23, 22 + iLen)
	elseif (iMod == 5) then
		ranNum = random(1, 23 + iLen)
	end
	if (iMod ~= 0) then
		loadBackground(backgroundTable[ranNum])
		print("Level background (" .. ranNum .. "/22): \"" .. backgroundTable[ranNum] .. "\"")
	end
end


--------------------------------------------------------------------------------
-- Some trigonometric functions.
--

-- returns the hyperbolic cosine of an angle
function cosh(fAng)
	return (exp(fAng) + exp(-fAng)) / 2
end


-- returns the hyperbolic sine of an angle
function sinh(fAng)
	return (exp(fAng) - exp(-fAng)) / 2
end


-- returns the hyperbolic tangent of an angle
function tanh(fAng)
	return (exp(fAng) - exp(-fAng)) / (exp(fAng) + exp(-fAng))
end


-- returns the hyperbolic cosecant of an angle
function csch(fAng)
	return 1 / sinh(fAng)
end


-- returns the hyperbolic secant of an angle
function sech(fAng)
	return 1 / cosh(fAng)
end


-- returns the hyperbolic cotangent of an angle
function coth(fAng)
	return 1 / tanh(fAng)
end


-- returns the cosecant of an angle
function csc(fAng)
	return 1 / sin(fAng)
end


-- returns the secant of an angle
function sec(fAng)
	return 1 / cos(fAng)
end


-- returns the cotangent of an angle
function cot(fAng)
	return 1 / tan(fAng)
end


-- returns the exsecant of an angle
function exsec(fAng)
	return sec(fAng) - 1
end


-- returns the coexsecant of an angle
function coexsec(fAng)
	return csc(fAng) - 1
end


-- returns the versesine of an angle
function vers(fAng)
	return 1 - cos(fAng)
end


-- returns the coversesine of an angle
function covers(fAng)
	return 1 - sin(fAng)
end


-- returns the half-versesine of an angle
function hav(fAng)
	return vers(fAng) / 2
end


--------------------------------------------------------------------------------
-- Some random number functions.
--

-- Randomly returns either 1 or -1.
function randomSign()
	local ranNum = random()
	if (ranNum > 0.5) then
		return 1
	else
		return -1
	end
end


-- Randomly returns either 1 or 0.
function randomBit()
	local ranNum = random()
	if (ranNum > 0.5) then
		return 1
	else
		return 0
	end
end


-- Works just like random(), but can accept zero as an argument.
function random2(fVal1, fVal2)
	if (fVal2) then
		if ((fVal2 - fVal1) == 0) then
			return fVal2
		else
			return random(fVal1, fVal2)
		end
	elseif (fVal1) then
		if (fVal1 == 0) then
			return 0
		else
			return random(fVal1)
		end
	else
		return random()
	end
end


-- Works just like random(), but can accept zero as an argument and always returns a float value, not an integer.
function random3(fVal1, fVal2)
	if (fVal2) then
		local tmpVal = random() * (fVal2 - fVal1)
		return fVal1 + tmpVal
	elseif (fVal1) then
		return random() * fVal1
	else
		return random()
	end
end


--------------------------------------------------------------------------------
-- Some miscellaneous mathematical functions.
--

-- Rounds a number to the nearest integer.
function round(fVal)
	return floor(fVal + 0.5)
end


--------------------------------------------------------------------------------
-- Some vector functions.
--

-- returns the normalized form of a vector
function vnormalize(tVec)
	local tmpVal = vlength(tVec)
	local tmpVec = vdivide(tVec, tmpVal)
	return tmpVec
end

-- returns the length of a vector
function vlength(tVec)
	local tmpVal = sqrt(vsum(vpower(tVec, 2)))
	return tmpVal
end

-- returns the distance between two vectors
function vdistance(tVec1, tVec2)
	local tmpVal = vlength(vsubtractV(tVec2, tVec1))
	return tmpVal
end

-- returns the dot product of two vectors
function vdot(tVec1, tVec2)
	local tmpVal = vsum(vmultiplyV(tVec1, tVec2))
	return tmpVal
end

-- returns the angle between two vectors
function vangle(tVec1, tVec2)
	local tmpVal = acos(vdot(vnormalize(tVec1), vnormalize(tVec2)))
	return tmpVal
end

-- returns the cross product of two vectors as a new vector
function vcross(tVec1, tVec2)
	local tmpVec =
	{
		tVec1[2] * tVec2[3] - tVec1[3] * tVec2[2],
		tVec1[3] * tVec2[1] - tVec1[1] * tVec2[3],
		tVec1[1] * tVec2[2] - tVec1[2] * tVec2[1],
	}
	return tmpVec
end

-- adds "fVal" to each vector component, then returns the resulting vector.
function vadd(tVec, fVal)
	local tmpVec = {}
	for i, tTab in tVec do
		tmpVec[i] = tVec[i] + fVal
	end
	return tmpVec
end

-- adds the components of vector 2 to the components of vector 1, then returns the resulting vector.
function vaddV(tVec1, tVec2)
	local tmpVec = {}
	for i, tTab in tVec2 do
		tmpVec[i] = tVec1[i] + tTab
	end
	return tmpVec
end

-- subtracts "fVal" from each vector component, then returns the resulting vector.
function vsubtract(tVec, fVal)
	local tmpVec = {}
	for i, tTab in tVec do
		tmpVec[i] = tVec[i] - fVal
	end
	return tmpVec
end

-- subtracts the components of vector 2 from the components of vector 1, then returns the resulting vector.
function vsubtractV(tVec1, tVec2)
	local tmpVec = {}
	for i, tTab in tVec2 do
		tmpVec[i] = tVec1[i] + tTab
	end
	return tmpVec
end

-- multiplies each vector component by "fVal", then returns the resulting vector.
function vmultiply(tVec, fVal)
	local tmpVec = {}
	for i, tTab in tVec do
		tmpVec[i] = tTab * fVal
	end
	return tmpVec
end

-- multiplies the components of vector 1 by the components of vector 2, then returns the resulting vector.
function vmultiplyV(tVec1, tVec2)
	local tmpVec = {}
	for i, tTab in tVec2 do
		tmpVec[i] = tVec1[i] * tTab
	end
	return tmpVec
end

-- divides each vector component by "fVal", then returns the resulting vector.
function vdivide(tVec, fVal)
	local tmpVec = {}
	for i, tTab in tVec do
		tmpVec[i] = tTab / fVal
	end
	return tmpVec
end

-- divides the components of vector 1 by the components of vector 2, then returns the resulting vector.
function vdivideV(tVec1, tVec2)
	local tmpVec = {}
	for i, tTab in tVec2 do
		tmpVec[i] = tVec1[i] / tTab
	end
	return tmpVec
end

-- raises each vector component to the power "fVal", then returns the new vector.
function vpower(tVec, fVal)
	local tmpVec = {}
	for i, tTab in tVec do
		tmpVec[i] = tTab^fVal
	end
	return tmpVec
end

-- raises the components of vector 1 to the power specified using the components vector 2, then returns the new vector.
function vpowerV(tVec1, tVec2)
	local tmpVec = {}
	for i, tTab in tVec2 do
		tmpVec[i] = tVec1[i]^tTab
	end
	return tmpVec
end

-- returns the sum of all vector components
function vsum(tVec1)
	local tmpVal = 0
	for i, tTab in tVec1 do
		tmpVal = tmpVal + tTab
	end
	return tmpVal
end

-- rotates a vector around the origin by the specified Euler angles, then returns the new vector.
-- rotates around the Z-axis first, then the X-axis, then the Y-axis.
function vrotate(tVec, tAng)
	local phi, PosX, PosY, PosZ = 0, tVec[1], tVec[2], tVec[3]
	phi = tAng[3]
	local zPosX = PosX * cos(phi) - PosY * sin(phi)
	local zPosY = PosX * sin(phi) + PosY * cos(phi)
	local zPosZ = PosZ * 1
	phi = tAng[1]
	local xPosX = zPosX * 1
	local xPosY = zPosY * cos(phi) - zPosZ * sin(phi)
	local xPosZ = zPosY * sin(phi) + zPosZ * cos(phi)
	phi = tAng[2]
	local yPosX = xPosX * cos(phi) + xPosZ * sin(phi)
	local yPosY = xPosY * 1
	local yPosZ = xPosX * -1 * sin(phi) + xPosZ * cos(phi)
	local tmpVec = {yPosX, yPosY, yPosZ,}
	return tmpVec
end

-- returns a vector converted into a string
function vstr(tVec)
	local tmpStr = "{"
	for i, tTab in tVec do
		tmpStr = tmpStr .. tTab .. ", "
	end
	tmpStr = tmpStr .. "}\n"
	return tmpStr
end

-- returns an array containing the vector's Euler angles, relative to the Z-axis.
-- to reproduce the original vector, rotate a point on the Z-axis by these angles.
function vanglesXY(tVec2)
	local fSgnX, fSgnY = 1, 1
	local tPrjB1 = vnormalize({tVec2[1], 0, tVec2[3],})
	if (tPrjB1[1] ~= 0) then
		fSgnX = tPrjB1[1] / abs(tPrjB1[1]) * -1
	end
	local fAngY = acos(tPrjB1[3]) * fSgnX
	local tRotB1 = vrotate(tVec2, {0,fAngY,  0,})
	local tPrjB2 = vnormalize(tRotB1)
	if (tPrjB2[2] ~= 0) then
		fSgnY = tPrjB2[2] / abs(tPrjB2[2])
	end
	local fAngX = acos(tPrjB2[3]) * fSgnY
	return {fAngX, fAngY, 0,}
end

-- rotates vector 1 around vector 2 by the specified amount, then returns the new vector.
function vaxis_rotate(tVec1, tVec2, fAngZ)
	local tAng = vanglesXY(tVec2)
	local tRotA1 = vrotate(tVec1, {0, tAng[2], 0,})
	local tRotA2 = vrotate(tRotA1, {tAng[1], 0, 0,})
	local tRotC1 = vrotate(tRotA2, {0, 0, fAngZ,})
	local tRotC2 = vrotate(tRotC1, vmultiply(tAng, -1))
	return tRotC2
end


--------------------------------------------------------------------------------
-- Some table manipulation functions.
--

--------------------------------------------------------------------------------
-- Returns the length of a table. Useful where the 'getn' function is normally unavailable.
function getn(tTable)
	local nCount = 0
	for i, iCount in tTable do
		nCount = nCount + 1
	end
	return nCount
end


--------------------------------------------------------------------------------
-- Inserts an item into a table. Useful where the 'tinsert' function is normally unavailable.
function tinsert(tTable, Arg1, Arg2)
	if (Arg2) then
		local TempTable = {}
		for i = Arg1, getn(tTable) do
			TempTable[i + 1] = tTable[i]
		end
		for i = Arg1, getn(tTable) do
			tTable[i + 1] = TempTable[i + 1]
		end
		tTable[Arg1] = Arg2
	else
		tTable[getn(tTable) + 1] = Arg1
	end
end

-- Compares two tables and returns true if they're equal and false if they're not.
function tcomp(tTable1, tTable2)
	local same = 1
	if (getn(tTable1) ~= getn(tTable2)) then
		same = 0
	else
		for i, k in tTable1 do
			if (type(tTable1[i]) == "table") and (type(tTable2[i]) == "table") then
				same = tcomp(tTable1[i], tTable2[i])
				if (same == 0) then
					break
				end
			elseif (tTable1[i] ~= tTable2[i]) then
				same = 0
				break
			end
		end
	end
	return same
end


function makeCuboid (tPar)
	local t = random()
	local l = random3(-tPar[2], tPar[2])
	local w = random3(-tPar[3], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local p = randomSign()
	if (t < 1/3) then
		l = L * p
	elseif (t < 2/3) then
		w = W * p
	elseif (t <= 1) then
		h = H * p
	end
	return
	{
		l,
		h,
		w,
	}
end

function makeEllipsoid (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local u = random3(180)
	local v = random3(360)
	return
	{
		L * cos(v) * sin(u),
		H * sin(v) * sin(u),
		W * cos(u),
	}
end

function makeCylinder (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local u = random3(180)
	local v = random3(360)
	return
	{
		L * cos(v),
		h,
		W * sin(v),
	}
end

function makeCone (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local v = random3(360)
	return
	{
		(1 - h / c) * L * cos(v) / 2,
		h,
		(1 - h / c) * W * sin(v) / 2,
	}
end

function makeToroid (tPar)
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local M = random3(tPar[6] - tPar[5], tPar[6])
	local v = random3(360)
	local o = random3(360)
	return
	{
		(tPar[2] + M * cos(v)) * cos(o),
		H * sin(v),
		(tPar[3] + M * cos(v)) * sin(o),
	}
end

function makeHelicoid (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local t = random()
	return
	{
		L * cos(t * tPar[6] * 360),
		tPar[4] * (2 * t - 1),
		W * sin(t * tPar[6] *  360),
	}
end

function makeParaboloid (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local v = random3(360)
	return
	{
		L * sqrt(h / 1000) * cos(v),
		h,
		W * sqrt(h / 1000) * sin(v),
	}
end

function makeHyperboloid (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local t = random()
	local v = random3(360)
	local p = randomSign()
	return
	{
		L * sqrt(1 + (t * p)^2) * cos(v),
		H * (t * p),
		W * sqrt(1 + (t * p)^2) * sin(v),
	}
end

function makeAstroid (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local v = random3(360)
	local o = random3(360)
	return
	{
		L * (cos(o) * cos(v))^3,
		H * (sin(v))^3,
		W * (sin(o) * cos(v))^3,
	}
end

function makeFunnel (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local t = random()
	local v = random3(360)
	return
	{
		L * t * cos(v),
		H * log(t) / 10,
		W * t * sin(v),
	}
end

function makeDini (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local v = random3(360)
	local u = random3(180)
	return
	{
		L * (cos(tPar[6] * v) * sin(u/2)),
		H * (cos(u/2) + log(tan(u/4)) + rad(tPar[6] * v) / (2 * PI)),
		W * (sin(tPar[6] * v) * sin(u/2)),
	}
end

function makeCorkscrew (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local v = random3(360)
	local u = random3(180)
	return
	{
		L * cos(v) * cos(u),
		H * rad(v) / (2 * PI),
		W * sin(v) * cos(u),
	}
end

function makeSeashell (tPar)
	local t = random()
	local o = random3(360)
	return
	{
		(tPar[5] / tPar[6] + (1 - t) * (1 + cos(o))) * tPar[2] * cos(tPar[6] * t * 360),
		tPar[4] * t^(1/2) * (2 * tPar[6] - 1) + tPar[3] * sin(o) * (1 - t),
		(tPar[5] / tPar[6] + (1 - t) * (1 + cos(o))) * tPar[2] * sin(tPar[6] * t * 360),
	}
end

function makeSineDisc (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local H = random3(tPar[4] - tPar[5], tPar[4])
	local t = random()
	local v = random3(360)
	return
	{
		L * t * cos(v),
		H * sin(t * 360 * tPar[6]),
		W * t * sin(v),
	}
end

function makeSinePlane (tPar)
	local s = random()
	local t = random()
	return
	{
		tPar[2] * s * 2 - tPar[2],
		tPar[4] * (sin(s * 360 * tPar[6]) + sin(t * 360 * tPar[6])) / 2,
		tPar[3] * t * 2 - tPar[3],
	}
end

function makeMoebius (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local v = random3(360)
	return
	{
		L * cos(v) + h * cos(v/2) * cos(v),
		h * sin(v/2),
		W * sin(v) + h * cos(v/2) * sin(v),
	}
end

function makeLissajous3D (tPar)
	local v = random3(360)
	local o = random3(360)
	local u = random3(180)
	return
	{
		tPar[2] * sin(v * tPar[6])		+ tPar[3] * sin(u) * cos(o),
		tPar[2] * sin(v * tPar[6] * tPar[4])	+ tPar[3] * sin(u) * sin(o),
		tPar[2] * sin(v * tPar[6] * tPar[5])	+ tPar[3] * cos(u),
	}
end

function makeKlein (tPar)
	local u = random3(360)
	local v = random3(180)
	local X = cos(u) * (cos(u/2) * (sqrt(2) + cos(v)) + sin(u/2) * sin(v) * cos(v))
	local Y = -1 * sin(u/2) * (sqrt(2) + cos(v)) + cos(u/2) * sin(v) * cos(v)
	local Z = sin(u) * (cos(u/2) * (sqrt(2) + cos(v)) + sin(u/2) * sin(v) * cos(v))
	return
	{
		X * tPar[2],
		Y * tPar[4],
		Z * tPar[3],
	}
end

function makeKlein8 (tPar)
	local u = random3(360)
	local v = random3(360)
	local X = (e + cos(u/2) * sin(v) - sin(u/2) * sin(v*2)) * cos(u)
	local Y = sin(u/2) * sin(v) + cos(u/2) * sin(v*2)
	local Z = (e + cos(u/2) * sin(v) - sin(u/2) * sin(v*2)) * sin(u)
	return
	{
		X * a,
		Y * c,
		Z * b,
	}
end

function makeKuen (tPar)
	local u = random3(180)
	local v = random3(360)
	local X = 2 * (cos(v) + rad(v) * sin(v)) * sin(u) / (1 + rad(v)^2 * sin(u)^2)
	local Y = log(tan(u/2)) + 2 * cos(u) / (1 + rad(v)^2 * sin(u)^2)
	local Z = 2 * (sin(v) - rad(v) * cos(v)) * sin(u) / (1 + rad(v)^2 * sin(u)^2)
	return
	{
		X * a,
		Y * c,
		Z * b,
	}
end

function makeBoy (tPar)
	local u = random3(360)
	local v = random3(180)
	local X = (2/3) * (cos(u) * cos(2 * v) + sqrt(2) * sin(u) * cos(v)) * cos(u) / (sqrt(2) - sin(2 * u) * sin(3 * v))
	local Y = sqrt(2) * cos(u)^2 / (sqrt(2) - sin(2 * u) * sin(2 * v))
	local Z = (2/3) * (cos(u) * sin(2 * v) - sqrt(2) * sin(u) * sin(v)) * cos(u) / (sqrt(2) - sin(2 * u) * sin(3 * v))
	return
	{
		X * a,
		Y * c,
		Z * b,
	}
end

function makeRectangle (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local l = random3(-tPar[2], tPar[2])
	local w = random3(-tPar[3], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local p = randomSign()
	if (t <= 1/2) then
		l = L * p
	elseif (t <= 1) then
		w = W * p
	end
	return
	{
		l,
		h,
		w,
	}
end

function makeEllipse (tPar)
	local L = random3(tPar[2] - tPar[5], tPar[2])
	local W = random3(tPar[3] - tPar[5], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local v = random3(360)
	return
	{
		L * cos(v),
		h,
		W * sin(v),
	}
end

function makeParabola (tPar)
	local w = random3(-tPar[3], tPar[3])
	local h = random3(-tPar[4], tPar[4])
	local p = randomSign()
	return
	{
		sqrt(4 * w * tPar[2]) * p + random3(-tPar[5]/2, tPar[5]/2),
		h,
		w + random3(-tPar[5]/2, tPar[5]/2),
	}
end

function makeHyperbola (tPar)
	local h = random3(-tPar[4], tPar[4])
	local v = random3(360)
	return
	{
		tPar[2] / cos(v) + random3(-tPar[5]/2, tPar[5]/2),
		h,
		tPar[3] * tan(v) + random3(-tPar[5]/2, tPar[5]/2),
	}
end

function makeHypotrochoid (tPar)
	local v = random3(360)
	local o = random3(360)
	local u = random3(180)
	return
	{
		(tPar[2] - tPar[3]) * cos(v * tPar[6]) + tPar[5] * cos((tPar[2] - tPar[3]) / tPar[3] * v * tPar[6])	+ tPar[4] * sin(u) * cos(o),
		0													+ tPar[4] * sin(u) * sin(o),
		(tPar[2] - tPar[3]) * sin(v * tPar[6]) - tPar[5] * sin((tPar[2] - tPar[3]) / tPar[3] * v * tPar[6])	+ tPar[4] * cos(u),
	}
end

function makeEpitrochoid (tPar)
	local v = random3(360)
	local o = random3(360)
	local u = random3(180)
	return
	{
		(tPar[2] + tPar[3]) * cos(v * tPar[6]) - tPar[5] * cos((tPar[2] + tPar[3]) / tPar[3] * v * tPar[6])	+ tPar[4] * sin(u) * cos(o),
		0													+ tPar[4] * sin(u) * sin(o),
		(tPar[2] + tPar[3]) * sin(v * tPar[6]) - tPar[5] * sin((tPar[2] + tPar[3]) / tPar[3] * v * tPar[6])	+ tPar[4] * cos(u),
	}
end

function makeLissajous2D (tPar)
	local v = random3(360)
	local o = random3(360)
	local u = random3(180)
	return
	{
		tPar[2] * sin(v * tPar[6] + tPar[3])	+ tPar[4] * sin(u) * cos(o),
		0					+ tPar[4] * sin(u) * sin(o),
		tPar[2] * sin(v * tPar[6] * tPar[5])	+ tPar[4] * cos(u),
	}
end
