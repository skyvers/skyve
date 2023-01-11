import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:skyve_flutter/main.dart';
import 'package:skyve_flutter/util/skyve_rest_client.dart';
import 'package:skyve_flutter/views/auto_log_in.dart';
import 'package:skyve_flutter/views/skyve_container.dart';
import 'package:skyve_flutter/views/skyve_edit_view.dart';
import 'package:skyve_flutter/views/skyve_list_view.dart';
import 'package:skyve_flutter/widgets/skyve_menu.dart';

final containerMetaDataProvider =
    FutureProvider<Map<String, dynamic>>((ref) async {
  return Future<Map<String, dynamic>>(() async {
    final SkyveRestClient client = SkyveRestClient();
    if (!client.loggedIn) {
      await client.login(
          customer: 'demo', username: 'admin', password: 'admin');
    }
    if (client.loggedIn) {
      final metadata = await client.metadata();
      return metadata;
    }
    return {};
  });
});

final containerMenuProvider = FutureProvider((ref) async {
  void addItems(String moduleName, List<dynamic> jsonMenuOrGroup,
      List<SkyveMenuItem> items) {
    for (Map<String, dynamic> jsonItem in jsonMenuOrGroup) {
      if (jsonItem.containsKey('edit')) {
        items.add(SkyveMenuData(
            label: jsonItem['edit'],
            icon: null, //jsonItem['fontIcon'],
            routeName: '/e',
            params: {'m': moduleName, 'd': jsonItem['document']}));
      } else if (jsonItem.containsKey('list')) {
        items.add(SkyveMenuData(
            label: jsonItem['list'],
            icon: null, // jsonItem['fontIcon'],
            routeName: '/l',
            params: {'m': moduleName, 'q': jsonItem['query']}));
      } else if (jsonItem.containsKey('group')) {
        final List<SkyveMenuItem> groupItems = [];
        items.add(
            SkyveMenuGroup(label: jsonItem['group'], children: groupItems));
        addItems(moduleName, jsonItem['items'], groupItems);
      }
    }
  }

  return Future<List<SkyveMenuModule>>(() async {
    // Prefer global menu variable (if defined)
    if (menu != null) {
      return menu!;
    }

    // Use metadata if there is no local menu
    final metadata = await ref.watch(containerMetaDataProvider.future);
    final List<SkyveMenuModule> result = [];
    final List<dynamic> jsonMenus = metadata['menus'];
    for (Map<String, dynamic> jsonModule in jsonMenus) {
      final List<SkyveMenuItem> items = [];
      var module = SkyveMenuModule(
          label: jsonModule['title'], open: jsonModule['open'], items: items);
      final List<dynamic> jsonMenu = jsonModule['menu'];
      addItems(jsonModule['module'], jsonMenu, items);
      result.add(module);
    }
    return result;
  });
});

final containerRouterProvider = Provider((ref) {
  FutureOr<String?> redirect(BuildContext context, GoRouterState state) {
    // Heading to the log in page, leave it alone
    final loggingIn = state.subloc.contains(AutoLogIn.routeName);
    if (loggingIn) {
      return null;
    }

    // Otherwise are we logged in?
    final loggedIn = SkyveRestClient().loggedIn;
    if (loggedIn) {
      // Continue to the requested view
      return null;
    } else {
      // Redirect to the login page
      // May need to pass in the intended destination route
      var loginUri = Uri(
          path: AutoLogIn.routeName,
          queryParameters: {'destination': state.location});
      return loginUri.toString();
    }
  }

  return GoRouter(initialLocation: '/', redirect: redirect, routes: [
    GoRoute(
      name: 'Login',
      path: AutoLogIn.routeName,
      builder: (context, state) {
        String? dest = state.queryParams['destination'];
        return AutoLogIn(destination: dest);
      },
    ),
    GoRoute(path: '/', builder: (context, state) => const SkyveContainer()),
    GoRoute(
        name: 'List',
        path: '/l',
        builder: (context, state) {
          final String m = state.queryParams['m']!;
          final String q = state.queryParams['q']!;
          return SkyveListView(m: m, q: q);
        }),
    GoRoute(
        name: 'Edit',
        path: '/e',
        builder: (context, state) {
          final String m = state.queryParams['m']!;
          final String d = state.queryParams['d']!;
          final String? i = state.queryParams['i'];
          return SkyveEditView(m: m, d: d, i: i);
        }),
  ]);
});
