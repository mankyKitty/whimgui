#version 130

uniform vec2 iResolution = vec2(640.,480.);

float sdfCircle(vec2 p) {
  return length(p) - 1.0;
}

void main() {

  vec2 p = -1.0 + 2.0*gl_FragCoord.xy / iResolution.xy;
  p.x *= iResolution.x/iResolution.y;

  float d = sdfCircle(p);

  vec3 col = vec3(1.0) - sign(d) * vec3(0.1, 0.4, 0.7);
  col *= abs(d);

  gl_FragColor = vec4(col, 1.0);
}
