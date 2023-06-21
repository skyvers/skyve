import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:skyve_flutter/widgets/skyve_view.dart';
import 'models/skyve_datasource_models.dart';
import 'models/skyve_menu_models.dart';
import 'util/skyve_providers.dart';

void main() {
  runApp(const ProviderScope(child: App()));
}

// Allow metadata to drive the menu structure
const List<SkyveModuleMenuModel>? menu = null;

// Allow metadata to drive the datasource definitions
const Map<String, SkyveDataSourceModel>? dataSources = null;

// Allow metadata to drive the view definitions
final Map<String, SkyveView> views = {};

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
