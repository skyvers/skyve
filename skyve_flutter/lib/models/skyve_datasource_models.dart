class SkyveDataSourceModel {
  final String module;
  final String document;
  final String title;
  final String? fontIcon;
  final bool canCreate;
  final bool canUpdate;
  final bool canDelete;
  final bool aggregate;
  final List<SkyveDataSourceFieldModel> fields;
  const SkyveDataSourceModel(
      {required this.module,
      required this.document,
      required this.title,
      this.fontIcon,
      this.canCreate = false,
      this.canUpdate = false,
      this.canDelete = false,
      this.aggregate = false,
      required this.fields});

  SkyveDataSourceModel.fromJson(Map<String, dynamic> json)
      : module = json['module'],
        document = json['document'],
        title = json['title'],
        fontIcon = json['fontIcon'],
        canCreate = json['canCreate'] ?? false,
        canUpdate = json['canUpdate'] ?? false,
        canDelete = json['canDelete'] ?? false,
        aggregate = json['aggregate'] ?? false,
        fields = SkyveDataSourceFieldModel.fromJsonList(json['fields']);
}

class SkyveDataSourceFieldModel {
  final String name;
  final String title;
  final String type;
  final String align;
  final int? length;
  const SkyveDataSourceFieldModel(
      {required this.name,
      required this.title,
      required this.type,
      required this.align,
      this.length});

  SkyveDataSourceFieldModel.fromJson(Map<String, dynamic> json)
      : name = json['name'],
        title = json['title'],
        type = json['type'],
        align = json['align'] ?? 'left',
        length = json['length'];

  static List<SkyveDataSourceFieldModel> fromJsonList(List<dynamic> json) {
    return List.generate(
        json.length, (index) => SkyveDataSourceFieldModel.fromJson(json[index]),
        growable: false);
  }
}
