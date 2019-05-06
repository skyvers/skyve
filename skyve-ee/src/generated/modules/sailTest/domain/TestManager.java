package modules.sailTest.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Test Manager
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class TestManager extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "sailTest";
	/** @hidden */
	public static final String DOCUMENT_NAME = "TestManager";

	/** @hidden */
	public static final String namePropertyName = "name";
	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String baseSailXmlPathPropertyName = "baseSailXmlPath";
	/** @hidden */
	public static final String pathToChromeDriverPropertyName = "pathToChromeDriver";
	/** @hidden */
	public static final String clearTextTestPasswordPropertyName = "clearTextTestPassword";

	/**
	 * Name
	 **/
	private String name;
	/**
	 * Module
	 **/
	private String moduleName;
	/**
	 * Base Path for SAIL XML
	 **/
	private String baseSailXmlPath;
	/**
	 * Path to Chrome Driver
	 **/
	private String pathToChromeDriver;
	/**
	 * Test Password
	 **/
	private String clearTextTestPassword;

	@Override
	@XmlTransient
	public String getBizModule() {
		return TestManager.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return TestManager.DOCUMENT_NAME;
	}

	public static TestManager newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{name}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof TestManager) && 
					this.getBizId().equals(((TestManager) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #baseSailXmlPath} accessor.
	 * @return	The value.
	 **/
	public String getBaseSailXmlPath() {
		return baseSailXmlPath;
	}

	/**
	 * {@link #baseSailXmlPath} mutator.
	 * @param baseSailXmlPath	The new value.
	 **/
	@XmlElement
	public void setBaseSailXmlPath(String baseSailXmlPath) {
		preset(baseSailXmlPathPropertyName, baseSailXmlPath);
		this.baseSailXmlPath = baseSailXmlPath;
	}

	/**
	 * {@link #pathToChromeDriver} accessor.
	 * @return	The value.
	 **/
	public String getPathToChromeDriver() {
		return pathToChromeDriver;
	}

	/**
	 * {@link #pathToChromeDriver} mutator.
	 * @param pathToChromeDriver	The new value.
	 **/
	@XmlElement
	public void setPathToChromeDriver(String pathToChromeDriver) {
		preset(pathToChromeDriverPropertyName, pathToChromeDriver);
		this.pathToChromeDriver = pathToChromeDriver;
	}

	/**
	 * {@link #clearTextTestPassword} accessor.
	 * @return	The value.
	 **/
	public String getClearTextTestPassword() {
		return clearTextTestPassword;
	}

	/**
	 * {@link #clearTextTestPassword} mutator.
	 * @param clearTextTestPassword	The new value.
	 **/
	@XmlElement
	public void setClearTextTestPassword(String clearTextTestPassword) {
		preset(clearTextTestPasswordPropertyName, clearTextTestPassword);
		this.clearTextTestPassword = clearTextTestPassword;
	}
}
