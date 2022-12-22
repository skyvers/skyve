import 'package:flutter/material.dart';

import '../pages/contact_list.dart';
import '../pages/contact_list_2.dart';

Widget _buildMenuItem(BuildContext context, Widget title, String routeName, String currentRoute) {
  var isSelected = routeName == currentRoute;

  return ListTile(
    title: title,
    selected: isSelected,
    onTap: () {
      if (isSelected) {
        Navigator.pop(context);
      } else {
        Navigator.pushReplacementNamed(context, routeName);
      }
    },
  );
}

Drawer buildDrawer(BuildContext context, String currentRoute) {
  return Drawer(
    child: ListView(
      children: [
        _buildMenuItem(
          context,
          const Text('Home'),
          '/',
          currentRoute,
        ),
        _buildMenuItem(
          context,
          const Text('Contact List'),
          ContactListPage.route,
          currentRoute,
        ),
        _buildMenuItem(
          context,
          const Text('Pluto Grid Transition'),
          ContactListPage2.route,
          currentRoute,
        ),
      ],
    ),
  );
}
