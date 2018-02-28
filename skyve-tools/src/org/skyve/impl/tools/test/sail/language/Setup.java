package org.skyve.impl.tools.test.sail.language;

import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.step.Call;
import org.skyve.impl.tools.test.sail.language.step.interaction.Action;
import org.skyve.impl.tools.test.sail.language.step.interaction.Menu;
import org.skyve.impl.tools.test.sail.language.step.interaction.Module;
import org.skyve.impl.tools.test.sail.language.step.interaction.browser.Navigate;
import org.skyve.impl.tools.test.sail.language.step.interaction.browser.Reload;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Setup extends Procedure {
	@Override
	@XmlElementRefs({@XmlElementRef(type = Call.class),
						@XmlElementRef(type = Module.class),
						@XmlElementRef(type = Action.class),
						@XmlElementRef(type = Menu.class),
						@XmlElementRef(type = Navigate.class),
						@XmlElementRef(type = Reload.class)})
	public List<Step> getSteps() {
		return super.getSteps();
	}
}
