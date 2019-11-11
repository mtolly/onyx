#version 330 core

struct BoxInfo {
    float cornerWidth; // how wide are the left and right faces
    float cornerHeight; // how deep is the top face
    float totalWidth; // image width
    float totalHeight; // image height
};

struct ConeInfo {
    uint segments;
};

struct ImageOrColor {
    uint type; // 1 = color, 2 = box, 3 = cone
    vec3 color; // for type = 1
    sampler2D image; // for type = 2 or 3
    BoxInfo box; // for type = 2
    ConeInfo cone; // for type = 3
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
flat in float TexSegment;

uniform vec3 viewPos;
uniform Material material;
uniform float alpha;
uniform float startFade;
uniform float endFade;

vec3 getColor(ImageOrColor ioc)
{
    if (ioc.type == 1u) {
        return ioc.color;
    } else if (ioc.type == 2u) {
        // TexSegment = 1 (top), 2 (left), 3 (front), 4 (right)
        float widthFrac = (ioc.box.totalWidth - ioc.box.cornerWidth * 2.0) / ioc.box.totalWidth;
        float heightFrac = (ioc.box.totalHeight - ioc.box.cornerHeight) / ioc.box.totalHeight;
        float sideDepthFrac = ioc.box.cornerWidth / ioc.box.totalWidth;
        float topDepthFrac = ioc.box.cornerHeight / ioc.box.totalHeight;
        if (round(TexSegment) == 1) {
            return vec3(texture(ioc.image, vec2
                ( TexCoords.x * widthFrac + sideDepthFrac
                , TexCoords.y * topDepthFrac + heightFrac
                )));
        } else if (round(TexSegment) == 2) {
            return vec3(texture(ioc.image, vec2
                ( TexCoords.x * sideDepthFrac
                , TexCoords.y * heightFrac
                )));
        } else if (round(TexSegment) == 3) {
            return vec3(texture(ioc.image, vec2
                ( TexCoords.x * widthFrac + sideDepthFrac
                , TexCoords.y * heightFrac
                )));
        } else if (round(TexSegment) == 4) {
            return vec3(texture(ioc.image, vec2
                ( TexCoords.x * sideDepthFrac + sideDepthFrac + widthFrac
                , TexCoords.y * heightFrac
                )));
        }
    } else if (ioc.type == 3u) {
        float withinSegmentX = (TexCoords.x - (TexSegment / ioc.cone.segments)) * ioc.cone.segments;
        float neg1ToPos1 = (withinSegmentX - 0.5) * 2.0;
        float stretched = neg1ToPos1 / (1.0 - TexCoords.y);
        float withinSegmentNew = (stretched / 2.0) + 0.5;
        float finalX = (withinSegmentNew / ioc.cone.segments) + (TexSegment / ioc.cone.segments);
        return vec3(texture(ioc.image, vec2(finalX, TexCoords.y)));
    }
    return vec3(1.0, 0.0, 1.0); // magenta on invalid input
}

void main()
{
    // get constant colors or texture samples
    vec3 diffuseSource = getColor(material.diffuse);
    vec3 specularSource = getColor(material.specular);

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
    float horizonFade = 1 - (gl_FragCoord.y - startFade) / (endFade - startFade);
    FragColor = vec4(result, alpha * horizonFade);
}
