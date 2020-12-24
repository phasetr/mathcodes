pub enum Init {
    Sin,
    Square,
}

impl Init {
    pub fn new(init_str: &str) -> Init {
        match init_str {
            "sin" => Init::Sin,
            "square" => Init::Square,
            _ => Init::Sin, // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }
}
