pub enum Limiter {
    Minmod,
    Superbee,
    VanLeer,
    VanAlbada,
    FluxLimiter,
}

impl Limiter {
    pub fn new(limiter_str: &str) -> Limiter {
        match limiter_str {
            "minmod" => Limiter::Minmod,
            "superbee" => Limiter::Superbee,
            "vanLeer" => Limiter::VanLeer,
            "vanAlbada" => Limiter::VanAlbada,
            "fluxLimiter" => Limiter::FluxLimiter,
            _ => Limiter::Minmod,
        }
    }
}
