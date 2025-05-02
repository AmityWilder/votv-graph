#version 330

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;

// Input uniform values
uniform sampler2D texture0;

// Output fragment color
out vec4 finalColor;

const vec2 Scale = vec2(0.25, 0.45);
const vec2 ScaleIn = vec2(4.0, 2.5)*0.75;
const vec4 HmdWarpParam = vec4(1.0, 0.22, 0.24, 0.0);
const vec4 ChromaAbParam = vec4(0.996, -0.004, 1.014, 0.0);
uniform float renderWidth = 800;
uniform float renderHeight = 450;
float offset = 0.0;
float frequency = renderHeight/3.0;

void main()
{
    // The following two variables need to be set per eye
    vec2 LensCenter = vec2(0.5, 0.5);
    vec2 ScreenCenter = vec2(0.5, 0.5);

    // Scales input texture coordinates for distortion: vec2 HmdWarp(vec2 fragTexCoord, vec2 LensCenter)
    vec2 theta = (fragTexCoord - LensCenter)*ScaleIn;   // Scales to [-1, 1]
    float rSq = theta.x*theta.x + theta.y*theta.y;
    vec2 theta1 = theta*(HmdWarpParam.x + HmdWarpParam.y*rSq + HmdWarpParam.z*rSq*rSq + HmdWarpParam.w*rSq*rSq*rSq);
    //vec2 tc = LensCenter + Scale*theta1;

    // Detect whether blue texture coordinates are out of range since these will scaled out the furthest
    vec2 thetaBlue = theta1*(ChromaAbParam.z + ChromaAbParam.w*rSq);
    vec2 tcBlue = LensCenter + Scale*thetaBlue;

    if (any(bvec2(clamp(tcBlue, ScreenCenter - vec2(0.5, 0.5), ScreenCenter + vec2(0.5, 0.5)) - tcBlue))) {
        finalColor = vec4(0.0, 0.0, 0.0, 0.0);
    } else {
        // Do blue texture lookup
        float blue = texture(texture0, tcBlue).b;

        // Do green lookup (no scaling)
        vec2 tcGreen = LensCenter + Scale*theta1;
        float green = texture(texture0, tcGreen).g;

        // Do red scale and lookup
        vec2 thetaRed = theta1*(ChromaAbParam.x + ChromaAbParam.y*rSq);
        vec2 tcRed = LensCenter + Scale*thetaRed;
        float red = texture(texture0, tcRed).r;

        // Scanlines method 2
        float globalPos = (tcBlue.y + offset) * frequency;
        float wavePos = mix(0.75, 1.0, cos((fract(globalPos) - 0.5)*3.14));

        finalColor = vec4(red, green, blue, wavePos);
    }
}

