class SkyveModuleMenu {
  final String module;
  final String title;
  final bool open;
  final List<SkyveMenuItem> items;

  const SkyveModuleMenu(
      {required this.module,
      required this.title,
      required this.open,
      required this.items});

  SkyveModuleMenu.fromJson(Map<String, dynamic> json)
      : module = json['module'],
        title = json['title'],
        open = json['open'],
        items = SkyveMenuItem.fromJsonList(json['module'], json['menu']);
}

abstract class SkyveMenuItem {
  const SkyveMenuItem({required this.title});

  final String title;

  static List<SkyveMenuItem> fromJsonList(String module, List<dynamic> json) {
    return List.generate(json.length, (index) {
      Map<String, dynamic> jsonItem = json[index];
      if (jsonItem.containsKey('edit')) {
        return SkyveNavigationMenuItem(
            title: jsonItem['edit'],
            icon: jsonItem['fontIcon'],
            path: '/e',
            params: {'m': module, 'd': jsonItem['document']});
      } else if (jsonItem.containsKey('list')) {
        if (jsonItem.containsKey('model')) {
          return SkyveNavigationMenuItem(
              title: jsonItem['list'],
              icon: jsonItem['fontIcon'],
              path: '/l',
              params: {
                'm': module,
                'd': jsonItem['document'],
                'q': jsonItem['model']
              });
        }
        return SkyveNavigationMenuItem(
            title: jsonItem['list'],
            icon: jsonItem['fontIcon'],
            path: '/l',
            params: {'m': module, 'q': jsonItem['query']});
      } else if (jsonItem.containsKey('group')) {
        return SkyveMenuGroup(
            title: jsonItem['group'],
            items: SkyveMenuItem.fromJsonList(module, jsonItem['items']));
      }
      return const SkyveNavigationMenuItem(title: 'Unknown', path: '/');
    }, growable: false);
  }
}

class SkyveMenuGroup extends SkyveMenuItem {
  final List<SkyveMenuItem> items;

  const SkyveMenuGroup({required super.title, required this.items});
}

class SkyveNavigationMenuItem extends SkyveMenuItem {
  final String path;
  final String? icon;
  final Map<String, dynamic>? params;

  const SkyveNavigationMenuItem(
      {required super.title, this.icon, required this.path, this.params});
}
