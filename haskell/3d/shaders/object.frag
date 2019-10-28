#version 330 core

struct ImageOrColor {
    bool isColor;
    sampler2D image;
    vec3 color;
};

struct Material {
    ImageOrColor diffuse;
    ImageOrColor specular;
    float shininess;
};

struct Light {
    vec3 position;

    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
};

uniform Light light;

out vec4 FragColor;
in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;

uniform vec3 viewPos;
uniform Material material;

void main()
{
    // get constant colors or texture samples
    vec3 diffuseSource = material.diffuse.isColor
        ? material.diffuse.color
        : vec3(texture(material.diffuse.image, TexCoords));
    vec3 specularSource = material.specular.isColor
        ? material.specular.color
        : vec3(texture(material.specular.image, TexCoords));

    // ambient
    vec3 ambient = light.ambient * diffuseSource;

    // diffuse
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(light.position - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = light.diffuse * diff * diffuseSource;

    // specular
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    vec3 specular = light.specular * (spec * specularSource);

    vec3 result = ambient + diffuse + specular;
    FragColor = vec4(result, 1.0);
}
