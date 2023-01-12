import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import '../main.dart';
import '../models/skyve_datasource_models.dart';
import '../models/skyve_menu_models.dart';
import '../util/skyve_rest_client.dart';
import '../views/auto_log_in.dart';
import '../views/skyve_container.dart';
import '../views/skyve_edit_view.dart';
import '../views/skyve_list_view.dart';

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
  // Prefer global menu variable (if defined)
  if (menu != null) {
    return menu!;
  }

  return Future<List<SkyveModuleMenu>>(() async {
    // Use metadata if there is no local menu
    final metadata = await ref.watch(containerMetaDataProvider.future);
    final List<dynamic> json = metadata['menus'];
    return List.generate(
        json.length, (index) => SkyveModuleMenu.fromJson(json[index]),
        growable: false);
  });
});

final containerDataSourceProvider =
    FutureProvider<Map<String, SkyveDataSource>>((ref) async {
  // Prefer global dataSource variable (if defined)
  if (dataSources != null) {
    return dataSources!;
  }

  // Use metadata if there is no local menu
  return Future<Map<String, SkyveDataSource>>(() async {
    final Map<String, dynamic> metadata =
        await ref.watch(containerMetaDataProvider.future);
    Map<String, dynamic> json = metadata['dataSources'];
    Map<String, SkyveDataSource> result = {};
    for (MapEntry<String, dynamic> entry in json.entries) {
      result[entry.key] = SkyveDataSource.fromJson(entry.value);
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
          final String? d = state.queryParams['d'];
          final String q = state.queryParams['q']!;
          return SkyveListView(m: m, d: d, q: q);
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
