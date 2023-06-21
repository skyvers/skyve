class SkyveModuleMenuModel {
  final String module;
  final String title;
  final bool open;
  final List<SkyveMenuItemModel> items;

  const SkyveModuleMenuModel(
      {required this.module,
      required this.title,
      required this.open,
      required this.items});

  SkyveModuleMenuModel.fromJson(Map<String, dynamic> json)
      : module = json['module'],
        title = json['title'],
        open = json['open'],
        items = SkyveMenuItemModel.fromJsonList(json['module'], json['menu']);
}

abstract class SkyveMenuItemModel {
  const SkyveMenuItemModel({required this.title});

  final String title;

  static List<SkyveMenuItemModel> fromJsonList(
      String module, List<dynamic> json) {
    return List.generate(json.length, (index) {
      Map<String, dynamic> jsonItem = json[index];
      if (jsonItem.containsKey('edit')) {
        return SkyveNavigationMenuItemModel(
            title: jsonItem['edit'],
            icon: jsonItem['fontIcon'],
            path: '/e',
            params: {'m': module, 'd': jsonItem['document']});
      } else if (jsonItem.containsKey('list')) {
        if (jsonItem.containsKey('model')) {
          return SkyveNavigationMenuItemModel(
              title: jsonItem['list'],
              icon: jsonItem['fontIcon'],
              path: '/l',
              params: {
                'm': module,
                'd': jsonItem['document'],
                'q': jsonItem['model']
              });
        }
        return SkyveNavigationMenuItemModel(
            title: jsonItem['list'],
            icon: jsonItem['fontIcon'],
            path: '/l',
            params: {'m': module, 'q': jsonItem['query']});
      } else if (jsonItem.containsKey('group')) {
        return SkyveMenuGroupModel(
            title: jsonItem['group'],
            items: SkyveMenuItemModel.fromJsonList(module, jsonItem['items']));
      }
      return const SkyveNavigationMenuItemModel(title: 'Unknown', path: '/');
    }, growable: false);
  }
}

class SkyveMenuGroupModel extends SkyveMenuItemModel {
  final List<SkyveMenuItemModel> items;

  const SkyveMenuGroupModel({required super.title, required this.items});
}

class SkyveNavigationMenuItemModel extends SkyveMenuItemModel {
  final String path;
  final String? icon;
  final Map<String, dynamic>? params;

  const SkyveNavigationMenuItemModel(
      {required super.title, this.icon, required this.path, this.params});
}
