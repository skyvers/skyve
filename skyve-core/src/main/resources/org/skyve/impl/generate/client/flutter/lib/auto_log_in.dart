import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:skyve_week_mat/util/skyve_rest_client.dart';

class AutoLogIn extends StatefulWidget {
  static const routeName = '/auto-log-in';

  late String? destination;

  AutoLogIn({super.key, this.destination});

  @override
  State<AutoLogIn> createState() => _AutoLogInState();
}

class _AutoLogInState extends State<AutoLogIn> {
  @override
  void initState() {
    super.initState();

    _doLogin();
  }

  Future<void> _doLogin() async {
    var restClient = SkyveRestClient();
    bool login = await restClient.login(
        customer: 'demo', password: 'admin', username: 'admin');

    if (login) {
      debugPrint('Login ok');

      if (!mounted) return;

      String dest = widget.destination!;
      GoRouter.of(context).go(dest);
    } else {
      debugPrint('Login failed');
    }
  }

  @override
  Widget build(BuildContext context) {
    return Container(color: Colors.orange);
  }
}
