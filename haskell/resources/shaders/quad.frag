#version 330 core
out vec4 FragColor;

in vec2 TexCoord;

uniform sampler2D inTexture;
uniform vec2 inResolution;

void main()
{
    FragColor = texture(inTexture, TexCoord);
}
