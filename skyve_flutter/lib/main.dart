import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'dart:async';
import 'util/skyve_rest_client.dart';
import 'views/auto_log_in.dart';
import 'views/skyve_edit_view.dart';
import 'views/skyve_list_view.dart';
import 'widgets/skyve_menu.dart';

void main() {
  runApp(App());
}

const menu = [
  SkyveMenuModule(label: 'Admin', items: [
    SkyveMenuData(
        label: 'Password', routeName: 'admin/ChangePassword', icon: null),
    SkyveMenuData(label: 'Contacts', routeName: 'admin/qContacts', icon: null),
  ]),
];

class App extends StatelessWidget {
  final List<GoRoute> _goRoutes = [
    GoRoute(
        path: '/admin/Contact',
        builder: (context, state) =>
            SkyveEditView(queryParams: state.queryParams)),
    GoRoute(
        path: '/admin/qContacts',
        builder: (context, state) =>
            SkyveListView(queryParams: state.queryParams)),
  ];

  late GoRouter _router;

  App({Key? key}) : super(key: key) {
    // Non generated routes here
    var comboRoutes = [
      GoRoute(
        name: 'Login',
        path: AutoLogIn.routeName,
        builder: (context, state) {
          String? dest = state.queryParams['destination'];
          return AutoLogIn(destination: dest);
        },
      ),
      GoRoute(
        path: '/',
        builder: (context, state) =>
            SkyveListView(queryParams: state.queryParams),
      ),
    ];

    // Appending the generated ones
    comboRoutes.addAll(_goRoutes);

    // Create our router
    _router = GoRouter(
      initialLocation: '/',
      redirect: redirect,
      routes: comboRoutes,
    );
  }

  static FutureOr<String?> redirect(BuildContext context, GoRouterState state) {
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

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp.router(
      title: 'Skyve',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      routerConfig: _router,
    );
  }
}
