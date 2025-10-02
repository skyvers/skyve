isc.defineClass('BizHeader', 'HLayout');
isc.BizHeader.addProperties({
    width: '100%',
    height: 50,
    backgroundColor: '#477db0'
});
isc.BizHeader.addMethods({
	initWidget: function () {
		this.Super('initWidget', arguments);

		// Header template
		this.addMember(isc.HTMLFlow.create({
			width: '100%',
			contents: ''
		}));

		const menuData = [
			{title: '<div style="display:flex;align-items:center;padding:10px;flex-direction:column">' + 
						(isc.BizUtil.userContactImageUrl ? 
							'<img src="' + isc.BizUtil.userContactImageUrl + '" style="width:64px;height:64px;border-radius:50%;margin-bottom:6px"/>' : 
							'<div style="width:64px;height:64px;border-radius:50%;background-color:#6c757d;color:white;display:flex;align-items:center;justify-content:center;margin-bottom:6px">' + isc.BizUtil.userContactInitials + '</div>') +
						'<div style="padding-bottom:6px">' + isc.BizUtil.userContactName + '</div><div style="color:#777;font-size:12px">' + isc.BizUtil.userName + '</div></div>',
				canSelect: false,
				enabled: false},
			{isSeparator: true},
			{title: '<strong>User Management</strong>', canSelect: false, enabled: false},
			{title: '<i class="fa-solid fa-user" style="margin:0 10px"></i>Profile', click: function() {window.location ='?a=e&m=admin&d=UserDashboard';}},
			{title: '<i class="fa-solid fa-user-shield" style="margin:0 10px"></i>Account', click: function() {window.location = '?a=e&m=admin&d=UserAccount';}},
			{isSeparator: true},
			{title: '<i class="fa-solid fa-power-off" style="margin:0 10px"></i>Logout', click: function() {window.location = 'loggedOut';}}
		];
		if (isc.BizUtil.canSwitchMode) {
			menuData.addList([
				{isSeparator: true},
				{title: '<strong>Switch mode</strong>', canSelect: false, enabled: false},
				{title: '<i class="fa-solid fa-retweet" style="margin:0 10px"></i>Switch Mode', click: function() {setUxUi();}}				
			]);
		}
		
		let avatar = '<div style="position:relative;cursor:pointer;display:inline-block;padding-top:10px">';
		if (isc.BizUtil.userContactImageUrl) {
			avatar += '<img src="' + isc.BizUtil.userContactImageUrl + '" style="width:32px;height:32px;border-radius:50%;object-fit:cover;"/>';			
		}
		else {
			avatar += '<div style="width:32px;height:32px;border-radius:50%;background-color:#6c757d;color:white;display:flex;align-items:center;justify-content:center">' + isc.BizUtil.userContactInitials + '</div>';
		}
		// badge markup
		avatar += '<span style="position:absolute;top:2px;right:-5px;background:red;color:white;font-size:12px;padding:2px 6px;border-radius:50%;">4</span>';
		avatar += '</div>';
				
		// User account menu
		this.addMember(isc.HTMLFlow.create({
			width: 50,
			contents: avatar,
			click: function() {
				this._userMenu.showNextTo(this, 'bottom');
			},
			_userMenu: isc.Menu.create({
				showShadow: true,
				shadowDepth: 10,
				width: 220,
				showIcons: false,
				showSubmenus: false,
				showKeys: false,
				data: menuData
			})
		}));
	},
	
	replace: function(icon, title, link, help) {
		const m = this.getMember(0);
		const heading = isc.BizUtil.headerTemplate.replace('{icon}', icon).replace('{title}', title).replace('{link}', link).replace('{help}', help);
		m.setContents(heading);
	}
});
