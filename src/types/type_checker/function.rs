use crate::types::WeakTypeKind;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) params: Vec<WeakTypeKind>,
    pub(crate) result: Option<WeakTypeKind>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        let mut eq_result = self.name == other.name && self.params.len() == other.params.len();

        if !eq_result {
            return false;
        };

        for i in 0..self.params.len() {
            eq_result = eq_result && self.params[i].ptr_eq(&other.params[i]);
        }

        eq_result = eq_result
            && match (&self.result, &other.result) {
                (Some(ty), Some(other_ty)) => ty.ptr_eq(other_ty),
                (None, None) => true,
                _ => false,
            };

        eq_result
    }
}
