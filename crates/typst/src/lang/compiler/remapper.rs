use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

use indexmap::IndexMap;
use typst_syntax::Span;

use crate::foundations::{Label, Value};
use crate::lang::compiled::{
    CompiledClosure, CompiledDynamicImport, CompiledDynamicModule,
};
use crate::lang::operands::{
    AccessId, ClosureId, Constant, LabelId, ModuleId, PatternId, Pointer, Register,
    SpanId,
};
use crate::utils::hash128;

use super::{DynamicImport, DynamicModule, RegisterGuard};

pub trait RemapperImpl<K, V>: Default {
    fn insert(&mut self, value: V) -> K;
    fn get(&self, key: &K) -> Option<&V>;
    fn get_index(&self, index: usize) -> &V;
    fn into_values(&self) -> Vec<V::CompiledValue>
    where
        V: Clone + IntoCompiledValue;
}

pub struct Remapper<K, V, Impl = UniqueRemapper<K, V>> {
    inner: Impl,
    _ty: PhantomData<(K, V)>,
}

impl<K, V, Impl: RemapperImpl<K, V>> Default for Remapper<K, V, Impl> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V, Impl: RemapperImpl<K, V>> Remapper<K, V, Impl> {
    /// Creates a new empty remapper.
    pub fn new() -> Self {
        Self { inner: Impl::default(), _ty: PhantomData }
    }

    /// Inserts a new value into the remapper.
    pub fn insert(&mut self, value: V) -> K {
        self.inner.insert(value)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn get_index(&self, index: usize) -> &V {
        self.inner.get_index(index)
    }

    pub fn into_values(&self) -> Vec<V::CompiledValue>
    where
        V: Clone + IntoCompiledValue,
    {
        self.inner.into_values()
    }
}

pub trait RemapperKey: Clone + Eq {
    fn as_raw(&self) -> u16;

    fn from_raw(raw: u16) -> Self;
}

impl RemapperKey for Constant {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for LabelId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for ClosureId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for AccessId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for PatternId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for SpanId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for Pointer {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RemapperKey for ModuleId {
    fn as_raw(&self) -> u16 {
        Self::as_raw(*self)
    }

    fn from_raw(raw: u16) -> Self {
        Self::new(raw)
    }
}

pub trait IntoCompiledValue {
    type CompiledValue;

    fn into_compiled_value(self) -> Self::CompiledValue;
}

impl<T: IntoCompiledValue> IntoCompiledValue for Option<T> {
    type CompiledValue = Option<T::CompiledValue>;

    fn into_compiled_value(self) -> Self::CompiledValue {
        self.map(IntoCompiledValue::into_compiled_value)
    }
}

impl IntoCompiledValue for Value {
    type CompiledValue = Self;

    fn into_compiled_value(self) -> Self::CompiledValue {
        self
    }
}

impl IntoCompiledValue for Label {
    type CompiledValue = Value;

    fn into_compiled_value(self) -> Self::CompiledValue {
        Value::Label(self)
    }
}

impl IntoCompiledValue for Span {
    type CompiledValue = Span;

    fn into_compiled_value(self) -> Self::CompiledValue {
        self
    }
}

impl IntoCompiledValue for CompiledClosure {
    type CompiledValue = CompiledClosure;

    fn into_compiled_value(self) -> Self::CompiledValue {
        self
    }
}

impl IntoCompiledValue for RegisterGuard {
    type CompiledValue = Register;

    fn into_compiled_value(self) -> Self::CompiledValue {
        self.into()
    }
}

impl IntoCompiledValue for DynamicModule {
    type CompiledValue = CompiledDynamicModule;

    fn into_compiled_value(self) -> Self::CompiledValue {
        CompiledDynamicModule {
            imports: self
                .imports
                .into_iter()
                .map(|(_, v)| v.into_compiled_value())
                .collect(),
        }
    }
}

impl IntoCompiledValue for DynamicImport {
    type CompiledValue = CompiledDynamicImport;

    fn into_compiled_value(self) -> Self::CompiledValue {
        CompiledDynamicImport {
            names: self.names.into_iter().map(|p| p.resolve()).collect(),
            location: self.location.into_compiled_value(),
            span: self.span,
        }
    }
}


pub struct UniqueRemapper<K: RemapperKey + Hash, V: Hash> {
    values: IndexMap<u128, (K, V)>,
    map: HashMap<K, u128>,
}

impl<K: RemapperKey + Hash, V: Hash> Default for UniqueRemapper<K, V> {
    fn default() -> Self {
        Self {
            values: IndexMap::default(),
            map: HashMap::default(),
        }
    }
}

impl<K: RemapperKey + Hash, V: Hash> RemapperImpl<K, V> for UniqueRemapper<K, V> {
    fn insert(&mut self, value: V) -> K {
        let hash = hash128(&value);
        let len = self.values.len();
        let (key, _) = self.values.entry(hash).or_insert_with(|| {
            let key = K::from_raw(len as u16);
            (key, value)
        });

        self.map.insert(key.clone(), hash);

        key.clone()
    }

    fn get(&self, key: &K) -> Option<&V> {
        let hash = self.map.get(key)?;
        self.values.get(hash).map(|(_, v)| v)
    }

    fn get_index(&self, index: usize) -> &V {
        self.values
            .get_index(index)
            .map(|(_, (_, v))| v)
            .expect("index out of bounds")
    }

    fn into_values(&self) -> Vec<<V>::CompiledValue>
    where
        V: Clone + IntoCompiledValue {
        let mut out = Vec::from_iter(
            self.values.values().map(|(_, v)| v.clone().into_compiled_value()),
        );

        // A vec will grow to the next power of two, so we shrink it to the actual size.
        out.shrink_to_fit();

        out
    }

}

pub struct SimpleRemapper<K: RemapperKey, V> {
    values: Vec<V>,
    _keys: PhantomData<K>,
}

impl<K: RemapperKey, V> Default for SimpleRemapper<K, V> {
    fn default() -> Self {
        Self {
            values: vec![],
            _keys: PhantomData,
        }
    }
}

impl<K: RemapperKey, V> RemapperImpl<K, V> for SimpleRemapper<K, V> {
    fn insert(&mut self, value: V) -> K {
        let key = K::from_raw(self.values.len() as u16);
        self.values.push(value);
        key
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.values.get(key.as_raw() as usize)
    }

    fn get_index(&self, index: usize) -> &V {
        self.values.get(index).expect("index out of bounds")
    }

    fn into_values(&self) -> Vec<<V>::CompiledValue>
    where
        V: Clone + IntoCompiledValue
    {
        self.values.iter().map(|v| v.clone().into_compiled_value()).collect()
    }
}
