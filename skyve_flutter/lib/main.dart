import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'util/skyve_providers.dart';
import 'widgets/skyve_menu.dart';

void main() {
  runApp(const ProviderScope(child: App()));
}

// Allow metadata to drive the menu structure
const List<SkyveMenuModule>? menu = null;

class App extends ConsumerWidget {
  const App({Key? key}) : super(key: key);

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final GoRouter router = ref.watch(containerRouterProvider);

    return MaterialApp.router(
      title: 'Skyve',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      routerConfig: router,
    );
  }
}
