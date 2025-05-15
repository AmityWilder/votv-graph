use raylib::prelude::*;

use crate::CAMERA_LENGTH_DEFAULT;

pub struct Orbiter {
    pub target: Vector3,
    pub length: f32,
    /// Vertical
    pub pitch: f32,
    /// Horizontal
    pub yaw: f32,
}

impl Default for Orbiter {
    fn default() -> Self {
        Self {
            target: Vector3::zero(),
            length: CAMERA_LENGTH_DEFAULT,
            pitch: 0.0,
            yaw: 0.0,
        }
    }
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
        let q = Quaternion::from_euler(self.pitch, self.yaw, 0.0);
        self.target + Vector3::new(0.0, 0.0, -self.length).rotate_by(q)
    }
}
