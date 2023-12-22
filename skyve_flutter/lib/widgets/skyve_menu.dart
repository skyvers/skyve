import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import '../models/skyve_menu_models.dart';
import '../util/skyve_providers.dart';
import 'skyve_network_image.dart';

class SkyveMenu extends ConsumerWidget {
  final bool inDrawer;

  const SkyveMenu({Key? key, required this.inDrawer}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final model = ref.watch(containerMenuProvider);
    return model.when(
        loading: () => const Center(child: CircularProgressIndicator()),
        error: (err, stack) => Center(child: Text('Error: $err')),
        data: (model) {
          ThemeData theme = Theme.of(context);
          WidgetsFlutterBinding.ensureInitialized();
          final DrawerHeader header = DrawerHeader(
            decoration: BoxDecoration(
              color: theme.primaryColor,
            ),
            child: const SkvyeNetworkImage(),
          );

          final List<Widget> list = List.empty(growable: true);
          final List<Widget> moduleList = List.empty(growable: true);

          for (SkyveModuleMenuModel module in model) {
            List<Widget> groupItems = [];
            for (SkyveMenuItemModel item in module.items) {
              if (item is SkyveMenuGroupModel) {
                list.add(_buildMenuGroup(context: context, item: item));
                groupItems.clear();
              } else if (item is SkyveNavigationMenuItemModel) {
                list.add(_buildMenuItem(context: context, item: item));
              }
            }
            moduleList.add(_buildMenuModule(
                context: context, module: module, moduleMenuItems: list));
            list.clear();
          }

          return Drawer(
              child: Column(children: [
            SizedBox(height: 180, child: header),
            Expanded(
                child: ListView(padding: EdgeInsets.zero, children: moduleList))
          ]));
        });
  }

  Widget _buildMenuModule(
      {required BuildContext context,
      required SkyveModuleMenuModel module,
      required List<Widget> moduleMenuItems}) {
    return ExpansionTile(
      maintainState: true,
      title: Text(module.title),
      initiallyExpanded: module.open,
      childrenPadding: const EdgeInsets.only(left: 10),
      // Doesn't require a riverpod notifier as we know there is only 1 menu listening
      onExpansionChanged: (value) => {module.open = value},
      children: moduleMenuItems.toList(),
    );
  }

  // Can call itself recursively to build nested Groups
  Widget _buildMenuGroup(
      {required BuildContext context, required SkyveMenuGroupModel item}) {
    List<Widget> groupItems = [];
    for (SkyveMenuItemModel i in item.items) {
      if (i is SkyveMenuGroupModel) {
        groupItems.add(_buildMenuGroup(context: context, item: i));
      } else if (i is SkyveNavigationMenuItemModel) {
        groupItems.add(_buildMenuItem(context: context, item: i));
      }
    }

    return ExpansionTile(
      maintainState: true,
      title: Text(item.title),
      initiallyExpanded: item.open,
      childrenPadding: const EdgeInsets.only(left: 10),
      // Doesn't require a riverpod notifier as we know there is only 1 menu listening
      onExpansionChanged: (value) => {item.open = value},
      children: groupItems.toList(),
    );
  }

  Widget _buildMenuItem(
      {required BuildContext context,
      required SkyveNavigationMenuItemModel item}) {
    final String currentLocation = GoRouter.of(context).location;
    final String newLocation =
        Uri(path: item.path, queryParameters: item.params).toString();
    var selected = (currentLocation == newLocation);

    return ListTile(
      title: Text(item.title),
      leading: null, // icon,
      selected: selected,
      onTap: () {
        if (selected) {
          // Close the side menu if its in a drawer
          if (inDrawer) {
            context.pop();
          }
        } else {
          // Go to the clicked menu item
          context.pushReplacement(
              Uri(path: item.path, queryParameters: item.params).toString());
        }
      },
    );
  }
}
