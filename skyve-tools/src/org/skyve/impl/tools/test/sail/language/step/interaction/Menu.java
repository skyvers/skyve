package org.skyve.impl.tools.test.sail.language.step.interaction;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Select a menu item within a module.
 * The menu path will be the menu name if the menu item is not in a group, otherwise it is the path
 * from the menu root, something like menuGroupName/menuItemName.
 * 
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Menu implements Step {
	private String moduleName;
	private String menuPath;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getMenuPath() {
		return menuPath;
	}

	@XmlAttribute(name = "path", required = true)
	public void setMenuPath(String menuPath) {
		this.menuPath = menuPath;
	}

	@Override
	public void execute(StringBuilder script) {
	}
}
