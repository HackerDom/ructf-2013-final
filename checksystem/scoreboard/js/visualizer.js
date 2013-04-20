group = -1;
$(document).ready(Load_Visual_Data);

var fadeOffTime = 4;
var roundInc = 0.2;
var timeout = 200;

function Load_Visual_Data() {
    $.ajaxSetup({ cache: false });  // Is it good? - It is!
    $.getJSON("json/ructfe.json", init);
}

function idToInd( id )
{
    for ( var i = 0; i < TEAMS.length; i++ )
    {
        if ( TEAMS[i].id == id )
            return i;
    }
    return -1;
}

function servIdToInd( id )
{
    for ( var i = 0; i < SERVICES.length; i++ )
    {
        if ( SERVICES[i].id == id )
            return i;
    }
    return -1;
}

function circle( cx, cy, r, color )
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.fillStyle = color;
    ctx.beginPath();
    ctx.arc(cx,cy,r,0,2*Math.PI);
    ctx.fill();
    ctx.stroke();
}

function getControlPoints(x0,y0,x1,y1,x2,y2,t){
    //  x0,y0,x1,y1 are the coordinates of the end (knot) pts of this segment
    //  x2,y2 is the next knot -- not connected here but needed to calculate p2
    //  p1 is the control point calculated here, from x1 back toward x0.
    //  p2 is the next control point, calculated here and returned to become the 
    //  next segment's p1.
    //  t is the 'tension' which controls how far the control points spread.
    
    //  Scaling factors: distances from this knot to the previous and following knots.
    var d01=Math.sqrt(Math.pow(x1-x0,2)+Math.pow(y1-y0,2));
    var d12=Math.sqrt(Math.pow(x2-x1,2)+Math.pow(y2-y1,2));
   
    var fa=t*d01/(d01+d12);
    var fb=t-fa;
  
    var p1x=x1+fa*(x0-x2);
    var p1y=y1+fa*(y0-y2);

    var p2x=x1-fb*(x0-x2);
    var p2y=y1-fb*(y0-y2);  
    
    return [p1x,p1y,p2x,p2y]
}

function drawSpline(ctx,pts,t, color, alpha){
    ctx.save();
    var cp=[];   // array of control points, as x0,y0,x1,y1,...
    var n=pts.length;

    for(var i=0;i<n-4;i+=2){
        cp=cp.concat(getControlPoints(pts[i],pts[i+1],pts[i+2],pts[i+3],pts[i+4],pts[i+5],t));
    }    
    for(var i=2;i<pts.length-5;i+=2){
        ctx.beginPath();
        ctx.moveTo(pts[i],pts[i+1]);
        ctx.bezierCurveTo(cp[2*i-2],cp[2*i-1],cp[2*i],cp[2*i+1],pts[i+2],pts[i+3]);
        ctx.stroke();
        ctx.closePath();
    }
    //  For open curves the first and last arcs are simple quadratics.
    var gradient = ctx.createLinearGradient(pts[0],pts[1],pts[2],pts[3]);
    gradient.addColorStop("0","rgba("+color[0]+", "+color[1]+", "+color[2]+", 0.0)");
    gradient.addColorStop("1.0","rgba("+color[0]+", "+color[1]+", "+color[2]+", "+(alpha*0.5)+")");
    ctx.strokeStyle =  gradient;
    ctx.beginPath();
    ctx.moveTo(pts[0],pts[1]);
    ctx.quadraticCurveTo(cp[0],cp[1],pts[2],pts[3]);
    ctx.stroke();
    ctx.closePath();
    
    var gradient = ctx.createLinearGradient(pts[n-2],pts[n-1],pts[n-4],pts[n-3]);
    gradient.addColorStop("0","rgba("+color[0]+", "+color[1]+", "+color[2]+", 1.0)");
    gradient.addColorStop("1.0","rgba("+color[0]+", "+color[1]+", "+color[2]+", "+(alpha*0.5)+")");
    ctx.strokeStyle =  gradient;
    ctx.beginPath();
    ctx.moveTo(pts[n-2],pts[n-1]);
    ctx.quadraticCurveTo(cp[2*n-10],cp[2*n-9],pts[n-4],pts[n-3]);
    ctx.stroke();
    ctx.closePath();
    ctx.restore();
}

function myDrawLine( x1, y1, x2, y2, color, alpha )
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.strokeStyle = "rgba("+color[0]+", "+color[1]+", "+color[2]+", "+alpha+")";
    ctx.beginPath();
    ctx.moveTo(x1,y1);
    ctx.lineTo(x2,y2);
    ctx.stroke();
}

function myCubicCurve( points, beta, color, alpha, step )
{
    var xPrev = beta * points[1].x + (1 - beta) * points[0].x;
    var yPrev = beta * points[1].y + (1 - beta) * points[0].y;
    var stop = false;
    for (var t = step; true; t += step)
    {
        if ( stop )
            break;
        if (t >= 1)
        {
            stop = true;
            t = 1;
        }
        xNext = beta * (points[4].x * t * t * t + points[3].x * t * t + points[2].x * t + points[1].x) + (1 - beta) * (points[0].x + (points[5].x - points[0].x) * t);
        yNext = beta * (points[4].y * t * t * t + points[3].y * t * t + points[2].y * t + points[1].y) + (1 - beta) * (points[0].y + (points[5].y - points[0].y) * t);
        myDrawLine(xPrev, yPrev, xNext, yNext, color, alpha);
        xPrev = xNext;
        yPrev = yNext;
    }
}

function DrawBSpline( points, beta, step, color, alpha )
{
  var deltaX = (points[points.length - 1].x - points[0].x) / (points.length - 1);
  var deltaY = (points[points.length - 1].y - points[0].y) / (points.length - 1);
  var start;
  var end;

  var p0 = new Object();
  p0.x = points[0].x;
  p0.y = points[0].y;
  var p1 = new Object();
  p1.x = points[1].x - points[0].x;
  p1.y = points[1].y - points[0].y;
  var p2 = new Object();
  p2.x = 0;
  p2.y = 0;
  var p3 = new Object();
  p3.x = (points[0].x - 2 * points[1].x + points[2].x) / 6;
  p3.y = (points[0].y - 2 * points[1].y + points[2].y) / 6;

  start = points[0];
  var end = new Object();
  end.x = points[0].x + deltaX;
  end.y = points[0].y + deltaY;
  myCubicCurve([start, p0, p1, p2, p3, end], beta, color, alpha, step);
  p0.x = points[points.length - 1].x;
  p0.y = points[points.length - 1].y;
  p1.x = points[points.length - 2].x - points[points.length - 1].x;
  p1.y = points[points.length - 2].y - points[points.length - 1].y;
  p2.x = 0;
  p2.y = 0;
  p3.x = (points[points.length - 1].x - 2 * points[points.length - 2].x + points[points.length - 3].x) / 6;
  p3.y = (points[points.length - 1].y - 2 * points[points.length - 2].y + points[points.length - 3].y) / 6;
  start = points[points.length - 1];
  end = new Object();
  end.x = points[0].x + deltaX * (points.length - 2);
  end.y = points[0].y + deltaY * (points.length - 2);
  myCubicCurve([start, p0, p1, p2, p3, end], beta, color, alpha, step);
}

/*function drawPrettyLine( points, color, alpha )
{
    //DrawBSpline( points, 1.0, 0.1, color, alpha );
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    drawSpline( ctx, [points[0].x, points[0].y, points[1].x, points[1].y, points[2].x, points[2].y], 1.0, color, alpha );
}*/

//VAR_1
/*function drawPrettyLine( points, color, alpha )
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.save();
    var gradient = ctx.createLinearGradient(points[0].x,points[0].y,points[2].x,points[2].y);
    gradient.addColorStop("0","rgba("+color[0]+", "+color[1]+", "+color[2]+", 0.0)");
    gradient.addColorStop("1.0","rgba("+color[0]+", "+color[1]+", "+color[2]+", "+alpha+")");
//  gradient.addColorStop("1.0","rgb("+color[0]+", "+color[1]+", "+color[2]+")");
    ctx.strokeStyle = gradient;
    ctx.beginPath();
    ctx.moveTo(points[0].x,points[0].y);
    ctx.quadraticCurveTo(points[1].x,points[1].y,points[2].x,points[2].y);
    ctx.stroke();
    ctx.restore();
}*/

function drawPrettyLine( ctx, points, color, alpha )
{
    ctx.shadowBlur=10;
//    ctx.shadowColor="rgba("+color[0]+", "+color[1]+", "+color[2]+", 1)";
    ctx.shadowColor="rgba("+color[0]+", "+color[1]+", "+color[2]+", "+alpha+")";
//  var gradient = ctx.createLinearGradient(points[0].x,points[0].y,points[2].x,points[2].y);
//  gradient.addColorStop("0","rgba("+color[0]+", "+color[1]+", "+color[2]+", 0.0)");
//  gradient.addColorStop("1.0","rgba("+color[0]+", "+color[1]+", "+color[2]+", "+alpha+")");
//  gradient.addColorStop("1.0","rgb("+color[0]+", "+color[1]+", "+color[2]+")");
    ctx.strokeStyle ="rgba("+color[0]+", "+color[1]+", "+color[2]+", " + alpha + " )";
//    ctx.strokeStyle = "rgba(255, 255, 255, 1)";
//  ctx.strokeStyle = "rgba(255, 255, 255, "+alpha+")";
    ctx.beginPath();
    ctx.moveTo(points[0].x,points[0].y);
    ctx.quadraticCurveTo(points[1].x,points[1].y,points[2].x,points[2].y);
    ctx.stroke();
}


function sqr( n )
{
    return n*n;
}

function getTeamRadius( i )
{
    return MIN_TEAM_R + ( MAX_TEAM_R - MIN_TEAM_R ) * sqr( TEAMS[i].score / MAX_SCORE );
}

function getAlpha( i )
{
    if ( MAX_ROUND == MIN_ROUND )
    {
        return 1.0;
    }
    else
    {
        return Math.floor((( i - MIN_ROUND ) / ( MAX_ROUND - MIN_ROUND )) * 1000) / 1000;
    }
}

function drawCircle( cx, cy, r, n, rows, rmin )
{
    var counts = [];
    var rs = [];

    var sum = 0;
    for ( var i = 0; i < rows; i++ )
    {
        rs[i] = rmin + (r - rmin) * (i+1)/rows;
        counts[i] = parseInt(Math.floor(n/rows));
        sum += counts[i];
    }
    i = 0;
    sum = n - sum;
    while (sum)
    {
        counts[i % rows]++;
        i++;
        sum--;
    }

    var results = [];

    for ( var j = 0; j < rows; j++ )
    {
//      results[j] = [];
        for ( var i = 0; i < counts[j]; i++ )
        {
            var point = new Object();
            point.x = cx + rs[j] * Math.cos(2 * Math.PI * i / counts[j]);
            point.y = cy + rs[j] * Math.sin(2 * Math.PI * i / counts[j]);
            results.push( point );
        }
    }
    return results;
}

function drawServices()
{
    var n = SERVICE_COORDS.length;
    for ( var i = 0; i < n; i++ )
    {
        var p = SERVICE_COORDS[i];
        circle( p.x, p.y, SERVICE_R2, SERVICE_COLORS[i] );
    }
}

function drawTeams()
{
    var n = TEAMS_COORDS.length;
    for ( var i = 0; i < n; i++ )
    {
        var p = TEAMS_COORDS[i];
        circle( p.x, p.y, getTeamRadius( i ), TEAM_COLOR );
        var c = document.getElementById("myCanvas");
        var ctx = c.getContext("2d");
        ctx.save();
        ctx.translate(CX + (p.x - CX) * 1.05, CY + (p.y - CY) * 1.05);
        ctx.rotate(Math.PI*2*i/n);
        ctx.fillStyle = TEAM_COLOR;
        ctx.font = 'bold 15px sans-serif';
        ctx.fillText(TEAMS[i].name, 0, 0);
        ctx.restore();
    }
}

function drawAttack( ctx, from, to, service, alpha )
{
    var points = [];
    points[0] = TEAMS_COORDS[from];
    points[1] = SERVICE_COORDS[servIdToInd(service)];
    points[2] = TEAMS_COORDS[to];
    drawPrettyLine( ctx, points, SERVICE_COLORS[servIdToInd(service)], alpha );
}

function drawAttacks()
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.save();
    var n = ATTACKS.length;
    for ( var i = 0; i < n; i++ )
    {
        var from = idToInd(ATTACKS[i].from);
        var to = idToInd(ATTACKS[i].to);
        var service = ATTACKS[i].service;
        if ( ATTACKS[i].round >= MIN_ROUND && ATTACKS[i].round < MAX_ROUND )
        {
            drawAttack( ctx, from, to, service, getAlpha( ATTACKS[i].round ));
        }
    }
    ctx.restore();
}

function isTeam( team )
{
    for ( var i = 0; i < TEAMS.length; i++ )
    {
        if ( TEAMS[i].id == team )
        {
            return true;
        }
    } 
    return false;
}


function isService( serv )
{
    for ( var i = 0; i < SERVICES.length; i++ )
    {
        if ( SERVICES[i].id == serv )
        {
            return true;
        }
    } 
    return false;
}

function drawTime()
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.save();
    ctx.translate(10, 20);
    ctx.fillStyle = "rgb(220,220,220)";
    ctx.font = '15px sans-serif';
    ctx.fillText(Math.round(MAX_ROUND), 0, 0);
    ctx.restore();
}

function drawVisualisation(nax_max)
{
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    ctx.fillStyle = "white";
    ctx.fillRect(0,0, 855, 650);
    drawTime();
    drawTeams();
    // drawServices();
    drawAttacks();
    startRound = endRound;
    MIN_ROUND = startRound - fadeOffTime;
    endRound = startRound + roundInc;
    MAX_ROUND = endRound;
    console.log(MIN_ROUND + ", " + nax_max);
    if (MIN_ROUND<=nax_max) {
        setTimeout(function(){drawVisualisation(nax_max)}, timeout);
    }
    else {
        Load_Visual_Data();
    }
}

function init( data )
{
    MIN_TEAM_R = 2.0;
    MAX_TEAM_R = 5.0;
    TEAM_R = 200.0;
    SERVICE_R = 0.0;
    SERVICE_R2 = 5.0;

    TEAM_ROWS = 1;

    CX = 855.0/2;
    CY = 300.0;
    SERVICE_COLORS = [
        [255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0], [255, 0, 255], [0, 255, 255], [0, 0, 0]
    ];
    TEAM_COLOR = 'rgb(0,0,0)';

    SERVICES = data.services;
    if ( group == -1 )
    {
        TMP_TEAMS = data.teams;
    }
    else
    {
        TMP_TEAMS = [];
        for ( var i = 0; i < data.teams.length; i++ )
        {
            if ( data.teams[i].group == group )
            {
                TMP_TEAMS.push( data.teams[i] );
            }
        }
    }
    TEAMS = [];
    for ( var i = 0; i < TMP_TEAMS.length; i++)
    {
        if ( TMP_TEAMS[i].score > 0 )
        {
            TEAMS.push( TMP_TEAMS[i] );
        }
    }

    MAX_SCORE = -1;
    for ( var i = 0; i < TEAMS.length; i++ )
    {
        if ( TEAMS[i].score > MAX_SCORE )
            MAX_SCORE = TEAMS[i].score;
    }
    ATTACKS = [];
    for ( var i = 0; i < data.attacks.length; i++ )
    {
        if ( isTeam( data.attacks[i].from ) && isTeam( data.attacks[i].to ) && isService( data.attacks[i].service ) )
        {
            ATTACKS.push( data.attacks[i] );
        }
    }

    TEAMS_COORDS = drawCircle( CX, CY, TEAM_R, TEAMS.length, TEAM_ROWS, 0 );
    SERVICE_COORDS = drawCircle( CX, CY, SERVICE_R, SERVICES.length, 1, 0 );
    startRound = parseFloat(data.minRound);
    MIN_ROUND = startRound - fadeOffTime;
    endRound = startRound + roundInc;
    MAX_ROUND = endRound;
    drawVisualisation(data.maxRound);
}
