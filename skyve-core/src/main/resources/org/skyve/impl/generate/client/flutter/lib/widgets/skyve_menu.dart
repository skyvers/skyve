import 'package:##PROJECT##/main.dart';
import 'package:##PROJECT##/widgets/skyve_network_image.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

class SkyveMenu extends StatelessWidget {
  const SkyveMenu(
      {Key? key, required this.currentRoute, required this.inDrawer})
      : super(key: key);

  final String currentRoute;
  final bool inDrawer;

  @override
  Widget build(BuildContext context) {
    ThemeData theme = Theme.of(context);
    WidgetsFlutterBinding.ensureInitialized();
    final DrawerHeader header = DrawerHeader(
      decoration: BoxDecoration(
        color: theme.primaryColor,
      ),
      child: const SkvyeNetworkImage(),
    );
    final List<Widget> list = List.empty(growable: true);
    final List<Widget> moduleList = List.empty(growable: true);

    for (SkyveMenuModule module in menu) {
      List<Widget> groupItems = [];
      for (SkyveMenuItem row in module.items) {
        if (row is SkyveMenuGroup) {
          list.add(_buildMenuGroup(
              context: context, title: row.label, menuItems: row.children));
          groupItems.clear();
        } else if (row is SkyveMenuData) {
          list.add(_buildMenuData(
              context: context,
              title: row.label,
              routeName: row.routeName,
              currentRoute: currentRoute,
              icon: row.icon));
        }
      }
      moduleList.add(_buildMenuModule(
          context: context,
          title: module.label,
          moduleMenuItems: list,
          currentRoute: currentRoute));
      list.clear();
    }

    return Drawer(
      child: Column(
        children: [
          SizedBox(height: 180, child: header),
          Expanded(
              child: ListView(padding: EdgeInsets.zero, children: moduleList))
        ],
      ),
    );
  }

  Widget _buildMenuModule(
      {required BuildContext context,
      required String title,
      required moduleMenuItems,
      required String currentRoute}) {
    return ExpansionTile(
      maintainState: true,
      title: Text(title),
      childrenPadding: const EdgeInsets.only(left: 10),
      children: moduleMenuItems.toList(),
    );
  }

  // Can call itself recursively to build nested Groups
  Widget _buildMenuGroup(
      {required BuildContext context,
      required String title,
      required menuItems}) {
    List<Widget> groupItems = [];
    for (SkyveMenuItem child in menuItems) {
      if (child is SkyveMenuGroup) {
        groupItems.add(_buildMenuGroup(
            context: context, title: child.label, menuItems: child.children));
      } else if (child is SkyveMenuData) {
        groupItems.add(_buildMenuData(
            context: context,
            title: child.label,
            routeName: child.routeName,
            currentRoute: currentRoute,
            icon: child.icon));
      }
    }

    return ExpansionTile(
      maintainState: true,
      title: Text(title),
      childrenPadding: const EdgeInsets.only(left: 10),
      children: groupItems.toList(),
    );
  }

  Widget _buildMenuData(
      {required BuildContext context,
      required String title,
      required String routeName,
      required String currentRoute,
      Icon? icon}) {
    var selected = (routeName == currentRoute);

    return ListTile(
      title: Text(title),
      leading: icon,
      selected: selected,
      onTap: () {

        if (!selected) {
          // Go to the clicked menu item
          GoRouter.of(context).go(routeName);
        } else {
          // Close the side menu
          GoRouter.of(context).pop();
        }
      },
    );
  }
}

class SkyveMenuModule {
  const SkyveMenuModule({required this.label, required this.items});

  final String label;
  final List<SkyveMenuItem> items;
}

class SkyveMenuItem {
  const SkyveMenuItem({required this.label});

  final String label;
}

class SkyveMenuGroup extends SkyveMenuItem {
  const SkyveMenuGroup({required super.label, required this.children});

  final List<SkyveMenuItem> children;
}

class SkyveMenuData extends SkyveMenuItem {
  const SkyveMenuData(
      {required super.label, required this.icon, required this.routeName});

  final String routeName;
  final Icon? icon;
}
