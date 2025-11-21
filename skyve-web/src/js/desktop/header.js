/**
 * Implements BizHeader in the UI.
 * @class BizHeader
 * @extends HLayout
 */
isc.defineClass("BizHeader", "HLayout");

isc.BizHeader.addProperties({
	width: "100%",
	height: 50,
	backgroundColor: "#477db0",
});

isc.BizHeader.addMethods({
	/**
	 * Initialise widget and construct UI.
	 */
	initWidget: function () {
		this.Super("initWidget", arguments);

		// Header template
		this.addMember(
			isc.HTMLFlow.create({
				width: "100%",
				contents: "",
			}),
		);

		// Helpers
		const userImg = isc.BizUtil.userContactImageUrl;
		const userInitials = isc.BizUtil.userContactInitials;
		const userName = isc.BizUtil.userName;
		const userDisplay = isc.BizUtil.userContactName;

		const avatar64 = userImg
			? `<img src="${userImg}" style="width:64px;height:64px;border-radius:50%;margin-bottom:6px"/>`
			: `<div style="width:64px;height:64px;border-radius:50%;background:#6c757d;color:#fff;
                 display:flex;align-items:center;justify-content:center;margin-bottom:6px">
                   ${userInitials}
               </div>`;

		const profileBlock = `
            <div style="display:flex;align-items:center;padding:10px;flex-direction:column">
                ${avatar64}
                <div style="padding-bottom:6px">${userDisplay}</div>
                <div style="color:#777;font-size:12px">${userName}</div>
            </div>
        `;

		// Menu data
		const menuData = [
			{ title: profileBlock, canSelect: false, enabled: false },
			{ isSeparator: true },

			{
				title: "<strong>User Management</strong>",
				canSelect: false,
				enabled: false,
			},
			{
				title: `<i class="fa-solid fa-user" style="margin:0 10px"></i>Profile`,
				click() {
					window.location = "?a=e&m=admin&d=UserDashboard";
				},
			},
			{
				title: `<i class="fa-solid fa-user-shield" style="margin:0 10px"></i>Account`,
				click() {
					window.location = "?a=e&m=admin&d=UserAccount";
				},
			},

			{ isSeparator: true },
			{
				title: `<i class="fa-solid fa-power-off" style="margin:0 10px"></i>Logout`,
				click() {
					window.location = "loggedOut";
				},
			},
		];

		if (isc.BizUtil.canSwitchMode) {
			menuData.addList([
				{ isSeparator: true },
				{ title: "<strong>Switch mode</strong>", canSelect: false, enabled: false },
				{
					title: `<i class="fa-solid fa-retweet" style="margin:0 10px"></i>Switch Mode`,
					click() {
						setUxUi();
					},
				},
			]);
		}

		// Avatar (top-right)
		const avatar32 = userImg
			? `<img src="${userImg}" style="width:32px;height:32px;border-radius:50%;object-fit:cover;"/>`
			: `<div style="width:32px;height:32px;border-radius:50%;background:#6c757d;color:#fff;
                display:flex;align-items:center;justify-content:center">${userInitials}</div>`;

		// Badge placeholder
		const avatarHtml = `
            <div style="position:relative;cursor:pointer;display:inline-block;padding-top:10px">
                ${avatar32}
                <span style="position:absolute;top:2px;right:-5px;background:red;color:white;font-size:12px;
                             padding:2px 6px;border-radius:50%;">4</span>
            </div>
        `;

		// User menu button
		this.addMember(
			isc.HTMLFlow.create({
				width: 50,
				contents: avatarHtml,
				click() {
					this._userMenu.showNextTo(this, "bottom");
				},
				_userMenu: isc.Menu.create({
					showShadow: true,
					shadowDepth: 10,
					width: 220,
					showIcons: false,
					showSubmenus: false,
					showKeys: false,
					data: menuData,
				}),
			}),
		);
	},

	/**
	 * Replace header contents using the template.
	 * @param {string} icon
	 * @param {string} title
	 * @param {string} link
	 * @param {string} help
	 */
	replace(icon, title, link, help) {
		const m = this.getMember(0);
		const heading = isc.BizUtil.headerTemplate
			.replace("{icon}", icon)
			.replace("{title}", title)
			.replace("{link}", link)
			.replace("{help}", help);

		m.setContents(heading);
	},
});
