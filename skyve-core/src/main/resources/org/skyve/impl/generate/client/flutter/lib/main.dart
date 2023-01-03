##IMPORTS##
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'auto_log_in.dart';
import 'dart:async';
import 'util/skyve_rest_client.dart';

void main() {
  runApp(App());
}

##MENU##

class App extends StatelessWidget {

  ##ROUTES##

  late final GoRouter _router;

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
        path: '/home',
        builder: (context, state) => AdminqContactsList(queryParams: state.queryParams),
      ),
    ];

    // Appending the generated ones
    comboRoutes.addAll(_goRoutes);

    // Create our router
    _router = GoRouter(
      initialLocation: '/home',
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
