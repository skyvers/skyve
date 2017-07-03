package modules.admin.util;

import modules.admin.domain.Tag;

public class TagFactoryExtension extends TagFactory {

	@Override
	public Tag getInstance() throws Exception {
		Tag tag = super.getInstance();
		tag.setActionTag(super.getInstance());

		return tag;
	}

}
