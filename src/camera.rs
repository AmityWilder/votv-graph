use raylib::{math::glam::Quat, prelude::*};

pub struct Orbiter {
    pub target: Vector3,
    pub length: f32,
    /// Vertical
    pub pitch: f32,
    /// Horizontal
    pub yaw: f32,
}

impl Orbiter {
    pub const fn new(target: Vector3, length: f32, pitch: f32, yaw: f32) -> Self {
        Self {
            target,
            length,
            pitch,
            yaw,
        }
    }

    pub fn position(&self) -> Vector3 {
        let q = Quat::from_euler(glam::EulerRot::XYZ, self.pitch, self.yaw, 0.0);
        self.target + q.mul_vec3(Vector3::new(0.0, 0.0, -self.length))
    }
}
