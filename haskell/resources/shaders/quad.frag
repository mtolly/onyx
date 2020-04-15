#version 330 core
out vec4 FragColor;

in vec2 TexCoord;

uniform sampler2D inTexture;
uniform vec2 inResolution;
uniform float startFade;
uniform float endFade;

void main()
{
    float horizonFade = 1.0 - (TexCoord.y - startFade) / (endFade - startFade);
    if (horizonFade > 1.0) horizonFade = 1.0;
    if (horizonFade < 0.0) horizonFade = 0.0;
    vec4 result = texture(inTexture, TexCoord);
    FragColor = vec4(result.rgb, result.a * horizonFade);
}
