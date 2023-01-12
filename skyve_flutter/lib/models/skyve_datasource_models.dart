class SkyveDataSource {
  final String module;
  final String document;
  final List<SkyveDataSourceField> fields;
  const SkyveDataSource(
      {required this.module, required this.document, required this.fields});

  SkyveDataSource.fromJson(Map<String, dynamic> json)
      : module = json['module'],
        document = json['document'],
        fields = SkyveDataSourceField.fromJsonList(json['fields']);
}

class SkyveDataSourceField {
  final String name;
  const SkyveDataSourceField({required this.name});

  SkyveDataSourceField.fromJson(Map<String, dynamic> json)
      : name = json['name'];

  static List<SkyveDataSourceField> fromJsonList(List<dynamic> json) {
    return List.generate(
        json.length, (index) => SkyveDataSourceField.fromJson(json[index]),
        growable: false);
  }
}
