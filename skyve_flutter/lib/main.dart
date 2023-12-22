import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'widgets/skyve_view.dart';
import 'models/skyve_datasource_models.dart';
import 'models/skyve_menu_models.dart';
import 'util/skyve_providers.dart';

void main() {
  runApp(const ProviderScope(child: App()));
}

// Allow metadata to drive the menu structure
List<SkyveModuleMenuModel> menu = [
  SkyveModuleMenuModel(module: 'admin', title: 'Admin', open: false, items: [
    SkyveMenuGroupModel(title: 'Communication', items: []),
    SkyveMenuGroupModel(title: 'Security Admin', items: []),
    SkyveMenuGroupModel(title: 'DevOps', items: []),
    SkyveMenuGroupModel(title: 'Reports', items: []),
  ]),
  SkyveModuleMenuModel(
      module: 'kitchensink', title: 'Kitchen Sink', open: false, items: [])
];

// Allow metadata to drive the datasource definitions
const Map<String, SkyveDataSourceModel>? dataSources = null;

// Allow metadata to drive the view definitions
final Map<String, SkyveView> views = {};

final List<GoRoute> goRoutes = [];

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
