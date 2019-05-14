uniform vec2 iResolution;

// The MIT License
// Copyright © 2017 Inigo Quilez
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


// Signed distance to a 2D rhombus

// List of some other 2D distances:
//
// Triangle:             https://www.shadertoy.com/view/XsXSz4
// Equilateral Triangle: https://www.shadertoy.com/view/Xl2yDW
// Isosceles Triangle:   https://www.shadertoy.com/view/MldcD7
// Rounded Rectangle:    https://www.shadertoy.com/view/4llXD7
// Rhombus:              https://www.shadertoy.com/view/XdXcRB
// Ellipse 1:            https://www.shadertoy.com/view/4sS3zz
// Ellipse 2:            https://www.shadertoy.com/view/4lsXDN
// Regular Pentagon:     https://www.shadertoy.com/view/llVyWW
// Trapezoid:            https://www.shadertoy.com/view/MlycD3
// Quadratic Bezier:     https://www.shadertoy.com/view/MlKcDD
// Uneven Capsule:       https://www.shadertoy.com/view/4lcBWn
// Vesica:               https://www.shadertoy.com/view/XtVfRW
// Cross:                https://www.shadertoy.com/view/XtGfzw
// Polygon:              https://www.shadertoy.com/view/wdBXRW
//
// and many more here:   http://www.iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm

float ndot(vec2 a, vec2 b ) { return a.x*b.x - a.y*b.y; }

float sdRhombus( in vec2 p, in vec2 b )
{
    vec2 q = abs(p);

    float h = clamp( (-2.0*ndot(q,b) + ndot(b,b) )/dot(b,b), -1.0, 1.0 );
    float d = length( q - 0.5*b*vec2(1.0-h,1.0+h) );
    d *= sign( q.x*b.y + q.y*b.x - b.x*b.y );

	return d;
}

float sdfCircle(vec2 p, float r) {
  return length(p) - r;
}

// void mainImage( out vec4 fragColor, in vec2 fragCoord )
void main()
{
	vec2 p = (2.0 * gl_FragCoord.xy - iResolution.xy) / iResolution.y;

	vec2 ra = vec2(0.0,6); // \'+ 0.3*cos( iTime + vec2(0.0,1.57) + 0.0 );

	float d = sdRhombus( p, ra );

  vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);
	col *= 1.0 - exp(-2.0*abs(d));
	col *= 0.8 + 0.2*cos(140.0*d);
	col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.02,abs(d)) );

	gl_FragColor = vec4(col, 1.0);
}
