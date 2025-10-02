package modules.admin.UserAccount;

import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.cache.StateUtil;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.Generic;
import modules.admin.domain.UserAccount;

public class UserAccountBizlet extends Bizlet<UserAccount> {

	@Override
	public UserAccount newInstance(UserAccount bean) throws Exception {
		// Populate the users current sessions
		final List<Generic> sessions = bean.getSessions();
		StateUtil.getSessions(CORE.getUser().getId()).forEach(s -> {
			Generic g = Generic.newInstance();
			g.setMemo1(s);
			sessions.add(g);
		});
		
		return bean;
	}
}
