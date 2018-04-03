package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.impl.generate.sail.Generator;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;

public class GenerateMenuSAIL extends GenerateSAIL {
	private static final long serialVersionUID = 3235202497294674514L;

	@Override
	protected Automation singular(User user, String moduleName, String uxui, UserAgentType userAgentType)
	throws Exception {
		return Generator.visitMenu(user, moduleName, uxui, userAgentType);
	}

	@Override
	protected List<Automation> plural(User user, String uxui, UserAgentType userAgentType)
	throws Exception {
		return Generator.visitMenus(user, uxui, userAgentType);
	}
}
