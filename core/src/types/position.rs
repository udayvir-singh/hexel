//! todo

/* -------------------- *
 *       POSITION       *
 * -------------------- */
/// Represents the position of an object in source code.
#[derive(Debug, Clone, Copy)]
pub struct Position {
    /// The starting byte offset of the position.
    pub byte_start: usize,
    /// The ending byte offset of the position.
    pub byte_end: usize,
    /// The starting line number of the position.
    pub line_start: u32,
    /// The ending line number of the position.
    pub line_end: u32,
    /// The starting column number of the position.
    pub col_start: u32,
    /// The ending column number of the position.
    pub col_end: u32,
}

impl Default for Position {
    fn default() -> Self {
        Self::new(0, 0, 1, 1, 0, 0)
    }
}

impl Position {
    /// Creates a new instance of [`Position`].
    #[inline]
    #[must_use]
    pub fn new(
        byte_start: usize, byte_end: usize,
        line_start: u32, line_end: u32,
        col_start: u32, col_end: u32,
    ) -> Self {
        Self {
            byte_start, byte_end,
            line_start, line_end,
            col_start,  col_end,
        }
    }
}


/* -------------------- *
 *     GET POSITION     *
 * -------------------- */
/// Provides methods to get information about the position of the implementing object.
pub trait GetPosition {
    /// Returns the starting byte offset of the position.
    #[must_use]
    fn byte_start(&self) -> usize;

    /// Returns the ending byte offset of the position.
    #[must_use]
    fn byte_end(&self) -> usize;

    /// Returns the starting line number of the position.
    #[must_use]
    fn line_start(&self) -> u32;

    /// Returns the ending line number of the position.
    #[must_use]
    fn line_end(&self) -> u32;

    /// Returns the starting column number of the position.
    #[must_use]
    fn col_start(&self) -> u32;

    /// Returns the ending column number of the position.
    #[must_use]
    fn col_end(&self) -> u32;

    /// Create a new [`Position`] instance from the position of the implementing object.
    #[inline]
    #[must_use]
    fn position(&self) -> Position {
        Position::new(
            self.byte_start(), self.byte_end(),
            self.line_start(), self.line_end(),
            self.col_start(),  self.col_end(),
        )
    }
}

impl GetPosition for Position {
    #[inline]
    fn byte_start(&self) -> usize {
        self.byte_start
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.byte_end
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.line_start
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.line_end
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.col_start
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.col_end
    }

    #[inline]
    fn position(&self) -> Position {
        *self
    }
}

impl<T: GetPosition> GetPosition for [T] {
    #[inline]
    fn byte_start(&self) -> usize {
        self[0].byte_start()
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self[self.len() - 1].byte_end()
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self[0].line_start()
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self[self.len() - 1].line_end()
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self[0].col_start()
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self[self.len() - 1].col_end()
    }
}


/* -------------------- *
 *    UPDATE POSITION   *
 * -------------------- */
/// todo
#[derive(Default, Debug, Clone, Copy)]
pub struct PositionDelta {
    /// todo
    pub add: bool,
    /// todo
    pub byte_delta: usize,
    /// todo
    pub line_delta: u32,
}

impl PositionDelta {
    /// todo
    pub fn new(add: bool, byte_delta: usize, line_delta: u32) -> Self {
        Self { add, byte_delta, line_delta }
    }

    /// todo
    pub fn is_zero(&self) -> bool {
        self.byte_delta | self.line_delta as usize == 0
    }
}

/// Provides a method to update the position of the implementing object.
pub trait UpdatePosition {
    /// Updates the position of the implementing object by the given [`PositionDelta`].
    fn update_position(&mut self, delta: PositionDelta);
}

impl UpdatePosition for Position {
    fn update_position(&mut self, delta: PositionDelta) {
        if delta.add {
            self.byte_start += delta.byte_delta;
            self.byte_end   += delta.byte_delta;
            self.line_start += delta.line_delta;
            self.line_end   += delta.line_delta;
        } else {
            self.byte_start -= delta.byte_delta;
            self.byte_end   -= delta.byte_delta;
            self.line_start -= delta.line_delta;
            self.line_end   -= delta.line_delta;
        }
    }
}

impl<T: UpdatePosition> UpdatePosition for [T] {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        for item in self {
            item.update_position(delta);
        }
    }
}
