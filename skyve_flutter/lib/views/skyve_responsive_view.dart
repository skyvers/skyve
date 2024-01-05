import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:nested_scroll_view_plus/nested_scroll_view_plus.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/util/skyve_providers.dart';
import 'package:skyve_flutter/util/skyve_rest_client.dart';
import '../util/skyve_flutter_form.dart';
import '../widgets/skyve_menu.dart';

mixin SkyveResponsiveView {
  // width of drawer/panel for menu
  static const double menuWidth = 240.0;

  // Used to track the screen size from the layout builder in responsiveView below
  // This is preferrable to using MediaQuery which has the potential to trigger unnecessary paints.
  static Size screenSize = const Size(1.0, 1.0); // NB no div 0

  // Is the width < the 'sm' breakpoint;
  static bool small = false;

  Widget responsiveView(
      {required BuildContext context,
      required String viewTitle,
      required Widget view}) {
    return LayoutBuilder(builder: (context, constraints) {
      screenSize = Size(constraints.maxWidth, constraints.maxHeight);
      small = (screenSize.width <= ResponsiveWidth.maxSmallScreenWidthPixels);
      final Drawer? drawer =
          small ? const Drawer(child: SkyveMenu(inDrawer: true)) : null;

      // Determine the body widget
      Widget body;
      if (small) {
        body = view;
      } else {
        body = Row(children: [
          const SizedBox(
              width: menuWidth,
              child: Column(
                  children: [Expanded(child: SkyveMenu(inDrawer: false))])),
          Expanded(child: view)
        ]);
      }

      ThemeData theme = Theme.of(context);

      return Scaffold(
          drawer: drawer,
          // NB Use NestedScrollViewPlus to allow over-stretch of SliverAppBar
// TODO In chrome having scrolling menu inlined causes - The provided ScrollController is currently attached to more than one ScrollPosition.
          body: NestedScrollViewPlus(
              headerSliverBuilder: (context, innerBoxIsScrolled) {
                return [
                  OverlapAbsorberPlus(
                    sliver: SliverAppBar(
                      pinned: true,
                      snap: true,
                      floating: true,
                      stretch: true,
                      toolbarHeight: 50.0,
                      actions: appBarActions(context),
                      expandedHeight: small ? 120.0 : 160.0,
// TODO use a leading icon when making this thing and the title and then add the tab bar in below
                      bottom: appBarBottomContents(context),
                      flexibleSpace: FlexibleSpaceBar(
                        title: Text(viewTitle),
                        stretchModes: const [
                          StretchMode.zoomBackground,
                          StretchMode.blurBackground,
                        ],
                        background: Stack(
                          fit: StackFit.expand,
                          children: [
                            DecoratedBox(
                              position: DecorationPosition.foreground,
                              decoration: BoxDecoration(
                                gradient: LinearGradient(
                                  colors: [
                                    theme.primaryColor,
                                    Colors.transparent,
                                  ],
                                  begin: Alignment.bottomCenter,
                                  end: Alignment.topCenter,
                                ),
                              ),
                              child: const Image(
                                image:
                                    AssetImage('assets/AppBarBackground.jpg'),
                                fit: BoxFit.cover,
                              ),
                            ),
                            Container(
                              padding: const EdgeInsets.all(32.0),
                              child: const Image(
                                image: AssetImage('assets/skyve_inv_logo.png'),
                                fit: BoxFit.contain,
                                opacity: AlwaysStoppedAnimation(0.3),
                              ),
                            ),
                          ],
                        ),
                      ),
                    ),
                  ),
                ];
              },
              body: SkyveFlutterForm(
                child: Form(
                  child: body,
                ),
              )),
          bottomNavigationBar: bottomNavigationBar(context));
    });
  }

  // Determine the bottom app bar
  // NB This is built inside a LayoutBuilder since it is responsive
  BottomAppBar? bottomNavigationBar(BuildContext context) {
    return null;
  }

  // Determine what the top app bar RHS contents should be, if anything.
  // The default implementation adds the avatar menu.
  // NB This is built inside a LayoutBuilder since it is responsive
  List<Widget>? appBarActions(BuildContext context) {
    String? avatarInitials;
    String? imageId;
    final Map<String, dynamic>? metadata =
        ProviderScope.containerOf(context, listen: false)
            .read(containerMetaDataProvider)
            .value;
    if (metadata == null) {
      avatarInitials = '??';
    } else {
      imageId = metadata['userContactImageId'];
      if (imageId == null) {
        avatarInitials = metadata['userContactAvatarInitials'] ?? '??';
      }
    }
    CircleAvatar avatar = (imageId == null)
        ? CircleAvatar(
            backgroundColor: Colors.brown.shade800,
            radius: 50.0,
            child: Text(avatarInitials!),
          )
        : CircleAvatar(
            backgroundImage: CachedNetworkImageProvider(
              // TODO CustomerResourceServlet requires a user in the session to serve content and we don't have one in phone mode
              SkyveRestClient.contentImageUrl(
                module: 'admin',
                document: 'Contact',
                binding: 'image',
                contentId: imageId,
                width: 50,
                height: 50,
              ),
            ),
          );

    return [
      SizedBox(
        width: 50.0,
        height: 50.0,
        child: PopupMenuButton<String>(
          icon: avatar,
          position: PopupMenuPosition.under,
          itemBuilder: (BuildContext context) {
            return [
              PopupMenuItem<String>(
                child: const Row(
                  children: [
                    Icon(Icons.logout, color: Colors.black45),
                    SizedBox(width: 10.0),
                    Text('Logout'),
                  ],
                ),
                onTap: () {
                  print('Logout');
                },
              ),
            ];
          },
        ),
      ),
    ];
  }

  // Determine what the top app bar bottom contents should be, if anything
  // NB This is built inside a LayoutBuilder since it is responsive
  PreferredSizeWidget? appBarBottomContents(BuildContext context) {
    return null;
  }
}
