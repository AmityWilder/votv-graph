#![allow(internal_features)]
#![feature(structural_match, intrinsics, rustc_attrs, const_trait_impl, core_intrinsics)]

use std::{cmp::Ordering::{self, *}, fmt, marker::StructuralPartialEq};

macro_rules! define_normal_type {
    ($(
        $(#[$m:meta])*
        $vis:vis struct $name:ident($float:ident as $uint:ident in $low:literal..=$high:literal);
    )+) => {$(
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[rustc_layout_scalar_valid_range_start($low)]
        #[rustc_layout_scalar_valid_range_end($high)]
        $(#[$m])*
        $vis struct $name($uint);

        #[allow(unused_comparisons)]
        const _: () = {
            assert!(<$uint>::MIN == 0);
            let ulow: $uint = $low;
            let uhigh: $uint = $high;
            assert!(ulow <= uhigh);

            assert!(size_of::<$float>() == size_of::<$uint>());

            assert!($low <= <$float>::MIN.to_bits() && <$float>::MIN.to_bits() <= $high);
            assert!($low <= <$float>::MAX.to_bits() && <$float>::MAX.to_bits() <= $high);
            assert!(!(<$float>::MIN.to_bits() < <$float>::INFINITY.to_bits() && <$float>::INFINITY.to_bits() < <$float>::MAX.to_bits()));
            assert!(!(<$float>::MIN.to_bits() < <$float>::NEG_INFINITY.to_bits() && <$float>::NEG_INFINITY.to_bits() < <$float>::MAX.to_bits()));
            assert!(!(<$float>::MIN.to_bits() < <$float>::NAN.to_bits() && <$float>::NAN.to_bits() < <$float>::MAX.to_bits()));
        };

        impl $name {
            #[inline]
            pub const fn new(val: $float) -> Option<Self> {
                if val.is_normal() {
                    Some(unsafe { Self(val.to_bits()) })
                } else if val == 0.0 {
                    Some(unsafe { Self(0) })
                } else {
                    None
                }
            }

            #[inline]
            pub const unsafe fn new_unchecked(val: $float) -> Self {
                unsafe { Self(val.to_bits()) }
            }

            #[inline]
            pub const fn get(self) -> $float {
                unsafe { std::intrinsics::transmute_unchecked(self) }
            }
        }

        impl StructuralPartialEq for $name {}

        impl PartialEq for $name {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                self.get() == other.get()
            }
        }

        impl Eq for $name {}

        impl Ord for $name {
            #[inline]
            fn cmp(&self, other: &Self) -> Ordering {
                let (a, b) = (self.get(), other.get());
                match (a <= b, a >= b) {
                    (false, false) => unreachable!(),
                    (false, true) => Greater,
                    (true, false) => Less,
                    (true, true) => Equal,
                }
            }

            #[inline]
            fn max(self, other: Self) -> Self {
                unsafe { Self::new_unchecked(self.get().max(other.get())) }
            }

            #[inline]
            fn min(self, other: Self) -> Self {
                unsafe { Self::new_unchecked(self.get().min(other.get())) }
            }

            #[inline]
            fn clamp(self, min: Self, max: Self) -> Self {
                unsafe { Self::new_unchecked(self.get().clamp(min.get(), max.get())) }
            }
        }

        impl PartialOrd for $name {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(Ord::cmp(self, other))
            }

            #[inline]
            fn lt(&self, other: &Self) -> bool {
                self.get() < other.get()
            }

            #[inline]
            fn le(&self, other: &Self) -> bool {
                self.get() <= other.get()
            }

            #[inline]
            fn gt(&self, other: &Self) -> bool {
                self.get() > other.get()
            }

            #[inline]
            fn ge(&self, other: &Self) -> bool {
                self.get() >= other.get()
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                <$float as fmt::Debug>::fmt(&self.get(), f)
            }
        }

        impl From<$name> for $float {
            #[inline]
            fn from(normal: $name) -> Self {
                // Call `get` method to keep range information.
                unsafe { std::intrinsics::transmute_unchecked(normal) }
            }
        }
    )+};
}

define_normal_type!{
    pub struct NormalF32Inner(f32 as u32 in 0..=0xFF7FFFFF);
    pub struct NormalF64Inner(f64 as u64 in 0..=0xFFEFFFFFFFFFFFFF);
}

/// A 32-bit float guaranteed to be normal.
///
/// Provides [`Ord`] and [`Eq`] but not [`Hash`].
pub type NormalF32 = NormalF32Inner;

/// A 64-bit float guaranteed to be normal.
///
/// Provides [`Ord`] and [`Eq`] but not [`Hash`].
pub type NormalF64 = NormalF64Inner;

const _: () = {
    assert!(std::mem::size_of::<Option<NormalF32>>() == std::mem::size_of::<NormalF32>());
    assert!(NormalF32::new(0.0).is_some());
    assert!(NormalF32::new(-0.0).is_some());
    assert!(NormalF32::new(1.0).is_some());
    assert!(NormalF32::new(-1.0).is_some());
    assert!(NormalF32::new(f32::MIN_POSITIVE).is_some());
    assert!(NormalF32::new(f32::MIN).is_some());
    assert!(NormalF32::new(f32::MAX).is_some());
    assert!(NormalF32::new(f32::NAN).is_none());
    assert!(NormalF32::new(f32::INFINITY).is_none());
    assert!(NormalF32::new(f32::NEG_INFINITY).is_none());
    assert!(NormalF32::new(1.0e-40_f32).is_none());
};
