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

          for (SkyveModuleMenu module in model) {
            List<Widget> groupItems = [];
            for (SkyveMenuItem item in module.items) {
              if (item is SkyveMenuGroup) {
                list.add(_buildMenuGroup(
                    context: context, title: item.title, items: item.items));
                groupItems.clear();
              } else if (item is SkyveNavigationMenuItem) {
                list.add(_buildMenuItem(
                    context: context,
                    title: item.title,
                    path: item.path,
                    params: item.params,
                    icon: item.icon));
              }
            }
            moduleList.add(_buildMenuModule(
                context: context,
                title: module.title,
                open: module.open,
                moduleMenuItems: list));
            list.clear();
          }

          return Drawer(
            child: Column(
              children: [
                SizedBox(height: 180, child: header),
                Expanded(
                    child: ListView(
                        padding: EdgeInsets.zero, children: moduleList))
              ],
            ),
          );
        });
  }

  Widget _buildMenuModule(
      {required BuildContext context,
      required String title,
      required bool open,
      required moduleMenuItems}) {
    return ExpansionTile(
      maintainState: true,
      title: Text(title),
      initiallyExpanded: open,
      childrenPadding: const EdgeInsets.only(left: 10),
      children: moduleMenuItems.toList(),
    );
  }

  // Can call itself recursively to build nested Groups
  Widget _buildMenuGroup(
      {required BuildContext context, required String title, required items}) {
    List<Widget> groupItems = [];
    for (SkyveMenuItem item in items) {
      if (item is SkyveMenuGroup) {
        groupItems.add(_buildMenuGroup(
            context: context, title: item.title, items: item.items));
      } else if (item is SkyveNavigationMenuItem) {
        groupItems.add(_buildMenuItem(
            context: context,
            title: item.title,
            path: item.path,
            params: item.params,
            icon: item.icon));
      }
    }

    return ExpansionTile(
      maintainState: true,
      title: Text(title),
      childrenPadding: const EdgeInsets.only(left: 10),
      children: groupItems.toList(),
    );
  }

  Widget _buildMenuItem(
      {required BuildContext context,
      required String title,
      required String path,
      required Map<String, dynamic>? params,
      String? icon}) {
    final String currentLocation = GoRouter.of(context).location;
    final String newLocation =
        Uri(path: path, queryParameters: params).toString();
    var selected = (currentLocation == newLocation);

    return ListTile(
      title: Text(title),
      leading: null, // icon,
      selected: selected,
      onTap: () {
        if (!selected) {
          // Go to the clicked menu item
          context.pushReplacement(
              Uri(path: path, queryParameters: params).toString());
        } else {
          // Close the side menu
          context.pop();
        }
      },
    );
  }
}
