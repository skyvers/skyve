import 'package:##PROJECT##/main.dart';
import 'package:flutter/material.dart';

class SkyveMenu extends StatelessWidget {
  const SkyveMenu(
      {Key? key, required this.currentRoute, required this.inDrawer})
      : super(key: key);

  final String currentRoute;
  final bool inDrawer;

  @override
  Widget build(BuildContext context) {
    ThemeData theme = Theme.of(context);

    final DrawerHeader header = DrawerHeader(
        decoration: BoxDecoration(
          color: theme.primaryColor,
        ),
        child:
            Image.asset('customer_image.png', height: 100, fit: BoxFit.cover));
    final List<Widget> list = List.filled(1, header, growable: true);

    for (SkyveMenuData item in menu) {
      list.add(_buildMenuItem(
          context: context,
          title: item.label,
          routeName: item.routeName,
          currentRoute: currentRoute));
    }

    return ListView(padding: EdgeInsets.zero, children: list);
  }

  Widget _buildMenuItem(
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
        Navigator.popUntil(context, (route) => (!Navigator.canPop(context)));

        if (!selected) {
          Navigator.pushReplacementNamed(context, routeName);
        }
      },
    );
  }
}

class SkyveMenuData {
  const SkyveMenuData(
      {this.icon, required this.label, required this.routeName});

  final String? icon;
  final String label;
  final String routeName;
}
