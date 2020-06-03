#version 330 core

struct ImageOrColor {
    uint type; // 1 = color, 2 = texture
    vec4 color; // for type = 1
    sampler2D image; // for type = 2
};

struct Material {
    ImageOrColor diffuse;
    ImageOrColor specular;
    float shininess;
};

struct Light {
    vec3 position;

    vec4 ambient;
    vec4 diffuse;
    vec4 specular;
};

uniform Light light;

out vec4 FragColor;
in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;

uniform vec3 viewPos;
uniform Material material;
uniform float alpha;
uniform float texScaleX;
uniform float texScaleY;
uniform float texScaleW;
uniform float texScaleH;

vec4 getColor(ImageOrColor ioc)
{
    if (ioc.type == 1u) {
        return ioc.color;
    } else if (ioc.type == 2u) {
        vec2 scaledCoords = TexCoords * vec2(texScaleW, texScaleH) + vec2(texScaleX, texScaleY);
        return texture(ioc.image, scaledCoords);
    }
    return vec4(1.0, 0.0, 1.0, 1.0); // magenta on invalid input
}

void main()
{
    // get constant colors or texture samples
    vec4 diffuseSource = getColor(material.diffuse);
    vec4 specularSource = getColor(material.specular);

    // ambient
    vec4 ambient = light.ambient * diffuseSource;

    // diffuse
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(light.position - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec4 diffuse = light.diffuse * diff * diffuseSource;

    // specular
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    vec4 specular = light.specular * (spec * specularSource);

    vec4 result = ambient + diffuse + specular;
    FragColor = vec4(result.rgb, result.a * alpha);
}
