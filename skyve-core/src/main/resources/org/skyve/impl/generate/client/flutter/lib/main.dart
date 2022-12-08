##IMPORTS##
import 'package:flutter/material.dart';

void main() {
  runApp(App());
}

##MENU##

class App extends StatelessWidget {
  App({Key? key}) : super(key: key);

  ##ROUTES##

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Skyve',
        theme: ThemeData(
          primarySwatch: Colors.blue,
        ),
        initialRoute: AdminContact.routeName,
        routes: routes);
  }
}
