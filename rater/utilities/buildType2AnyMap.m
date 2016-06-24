function map = buildType2AnyMap(type)
%BUILDTYPE2ANYMAP builds a map with keys of type 'type'.
%    map = BUILDTYPE2ANYMAP(type) constructs a map with 'type' type
%    keys and 'any' type values.

  map = containers.Map('KeyType', type, 'ValueType', 'any');
end
