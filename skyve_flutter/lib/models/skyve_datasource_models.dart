class SkyveDataSourceModel {
  final String module;
  final String document;
  final List<SkyveDataSourceFieldModel> fields;
  const SkyveDataSourceModel(
      {required this.module, required this.document, required this.fields});

  SkyveDataSourceModel.fromJson(Map<String, dynamic> json)
      : module = json['module'],
        document = json['document'],
        fields = SkyveDataSourceFieldModel.fromJsonList(json['fields']);
}

class SkyveDataSourceFieldModel {
  final String name;
  const SkyveDataSourceFieldModel({required this.name});

  SkyveDataSourceFieldModel.fromJson(Map<String, dynamic> json)
      : name = json['name'];

  static List<SkyveDataSourceFieldModel> fromJsonList(List<dynamic> json) {
    return List.generate(
        json.length, (index) => SkyveDataSourceFieldModel.fromJson(json[index]),
        growable: false);
  }
}
