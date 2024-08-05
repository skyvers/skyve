package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Control Panel
 * 
 * @depend - - - SailUserAgentType
 * @depend - - - SailTestStrategy
 * @depend - - - SailExecutor
 * @navhas n sailUser 0..1 UserProxy
 * @navhas n testDocumentNames 0..n ModuleDocument
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ControlPanel extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ControlPanel";

	/** @hidden */
	public static final String xmlTracePropertyName = "xmlTrace";

	/** @hidden */
	public static final String httpTracePropertyName = "httpTrace";

	/** @hidden */
	public static final String queryTracePropertyName = "queryTrace";

	/** @hidden */
	public static final String commandTracePropertyName = "commandTrace";

	/** @hidden */
	public static final String facesTracePropertyName = "facesTrace";

	/** @hidden */
	public static final String contentTracePropertyName = "contentTrace";

	/** @hidden */
	public static final String securityTracePropertyName = "securityTrace";

	/** @hidden */
	public static final String bizletTracePropertyName = "bizletTrace";

	/** @hidden */
	public static final String dirtyTracePropertyName = "dirtyTrace";

	/** @hidden */
	public static final String queryPropertyName = "query";

	/** @hidden */
	public static final String customerNameToSwapToPropertyName = "customerNameToSwapTo";

	/** @hidden */
	public static final String sailUserPropertyName = "sailUser";

	/** @hidden */
	public static final String sailModuleNamePropertyName = "sailModuleName";

	/** @hidden */
	public static final String sailUxUiPropertyName = "sailUxUi";

	/** @hidden */
	public static final String sailUserAgentTypePropertyName = "sailUserAgentType";

	/** @hidden */
	public static final String sailTestStrategyPropertyName = "sailTestStrategy";

	/** @hidden */
	public static final String sailExecutorPropertyName = "sailExecutor";

	/** @hidden */
	public static final String sailComponentBuilderPropertyName = "sailComponentBuilder";

	/** @hidden */
	public static final String sailLayoutBuilderPropertyName = "sailLayoutBuilder";

	/** @hidden */
	public static final String sailPropertyName = "sail";

	/** @hidden */
	public static final String sailLoginCustomerPropertyName = "sailLoginCustomer";

	/** @hidden */
	public static final String sailLoginPasswordPropertyName = "sailLoginPassword";

	/** @hidden */
	public static final String sailBaseUrlPropertyName = "sailBaseUrl";

	/** @hidden */
	public static final String resultsPropertyName = "results";

	/** @hidden */
	public static final String tabIndexPropertyName = "tabIndex";

	/** @hidden */
	public static final String selectedCachePropertyName = "selectedCache";

	/** @hidden */
	public static final String sessionCountPropertyName = "sessionCount";

	/** @hidden */
	public static final String testNumberToGeneratePropertyName = "testNumberToGenerate";

	/** @hidden */
	public static final String testModuleNamePropertyName = "testModuleName";

	/** @hidden */
	public static final String testTagNamePropertyName = "testTagName";

	/** @hidden */
	public static final String testTagGeneratedDataPropertyName = "testTagGeneratedData";

	/** @hidden */
	public static final String testDocumentNamesPropertyName = "testDocumentNames";

	/**
	 * User Agent Type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum SailUserAgentType implements Enumeration {
		desktop("desktop", "Desktop"),
		tablet("tablet", "Tablet"),
		phone("phone", "Phone"),
		other("other", "Other");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(SailUserAgentType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private SailUserAgentType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static SailUserAgentType fromCode(String code) {
			SailUserAgentType result = null;

			for (SailUserAgentType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static SailUserAgentType fromLocalisedDescription(String description) {
			SailUserAgentType result = null;

			for (SailUserAgentType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Test Strategy
	 * <br/>
	 * Assert (stop if they fail), Verify (test but don't stop), or None (don't conduct the tests at all)
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum SailTestStrategy implements Enumeration {
		Assert("Assert", "Assert"),
		Verify("Verify", "Verify"),
		None("None", "None");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(SailTestStrategy::toDomainValue).collect(Collectors.toUnmodifiableList());

		private SailTestStrategy(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static SailTestStrategy fromCode(String code) {
			SailTestStrategy result = null;

			for (SailTestStrategy value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static SailTestStrategy fromLocalisedDescription(String description) {
			SailTestStrategy result = null;

			for (SailTestStrategy value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Executor
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum SailExecutor implements Enumeration {
		primeFacesInlineSelenese("org.skyve.impl.sail.execution.PrimeFacesInlineSeleneseExecutor", "PrimeFaces Inline Selenese"),
		primeFacesInlineWebDriver("org.skyve.impl.sail.execution.PrimeFacesInlineWebDriverExecutor", "PrimeFaces Inline Web Driver");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(SailExecutor::toDomainValue).collect(Collectors.toUnmodifiableList());

		private SailExecutor(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static SailExecutor fromCode(String code) {
			SailExecutor result = null;

			for (SailExecutor value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static SailExecutor fromLocalisedDescription(String description) {
			SailExecutor result = null;

			for (SailExecutor value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * XML
	 * <br/>
	 * Log XML metadata parse operations
	 **/
	private Boolean xmlTrace;

	/**
	 * HTTP
	 * <br/>
	 * Log request information including headers, parameters, cache state and timings.
	 **/
	private Boolean httpTrace;

	/**
	 * Query
	 * <br/>
	 * Log BizQL, Document Queries, Metadata Queries generated and executed during processing.
	 **/
	private Boolean queryTrace;

	/**
	 * Command
	 * <br/>
	 * Log command information such as filter criteria and paging row counts.
	 **/
	private Boolean commandTrace;

	/**
	 * Faces
	 * <br/>
	 * Log the faces phases and the xhtml generated.
	 **/
	private Boolean facesTrace;

	/**
	 * Content
	 * <br/>
	 * Log information on content fetched and stored.
	 **/
	private Boolean contentTrace;

	/**
	 * Security
	 * <br/>
	 * Log information on security denials.
	 **/
	private Boolean securityTrace;

	/**
	 * Bizlet
	 * <br/>
	 * Log every bizlet callback made (verbose).
	 **/
	private Boolean bizletTrace;

	/**
	 * Dirty
	 * <br/>
	 * Log the dirty state of domain objects (verbose).
	 **/
	private Boolean dirtyTrace;

	/**
	 * BizQL
	 **/
	private String query;

	/**
	 * Customer Name To Swap To
	 **/
	private String customerNameToSwapTo;

	/**
	 * User
	 **/
	private UserProxyExtension sailUser = null;

	/**
	 * Module Name
	 **/
	private String sailModuleName;

	/**
	 * UX/UI
	 **/
	private String sailUxUi;

	/**
	 * User Agent Type
	 **/
	private SailUserAgentType sailUserAgentType;

	/**
	 * Test Strategy
	 * <br/>
	 * Assert (stop if they fail), Verify (test but don't stop), or None (don't conduct the tests at all)
	 **/
	private SailTestStrategy sailTestStrategy = SailTestStrategy.Assert;

	/**
	 * Executor
	 **/
	private SailExecutor sailExecutor;

	/**
	 * Component Builder
	 **/
	private String sailComponentBuilder = "org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain";

	/**
	 * Layout Builder
	 **/
	private String sailLayoutBuilder = "org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder";

	/**
	 * SAIL
	 **/
	private String sail;

	/**
	 * Sign In Customer
	 **/
	private String sailLoginCustomer;

	/**
	 * Sign In Password
	 **/
	private String sailLoginPassword;

	/**
	 * Base URL
	 **/
	private String sailBaseUrl;

	/**
	 * Results
	 **/
	private String results;

	/**
	 * TabIndex
	 * <br/>
	 * The index of the tab in the edit view.
			 	This is set to the results tab when there is results to display.
	 **/
	private Integer tabIndex;

	/**
	 * Cache
	 **/
	private String selectedCache;

	/**
	 * Session Count
	 **/
	private Integer sessionCount;

	/**
	 * Number To Generate
	 * <br/>
	 * admin.controlPanel.testnumberToGenerate.description
	 **/
	private Integer testNumberToGenerate = Integer.valueOf(1);

	/**
	 * Module Name
	 * <br/>
	 * The target module
	 **/
	private String testModuleName;

	/**
	 * Tag Name
	 * <br/>
	 * The name of the tag to be used
	 **/
	private String testTagName;

	/**
	 * Tag Generated Data?
	 **/
	private Boolean testTagGeneratedData = Boolean.valueOf(false);

	/**
	 * Document Names
	 **/
	private List<ModuleDocument> testDocumentNames = new ChangeTrackingArrayList<>("testDocumentNames", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ControlPanel.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ControlPanel.DOCUMENT_NAME;
	}

	public static ControlPanelExtension newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ControlPanel) && 
					this.getBizId().equals(((ControlPanel) o).getBizId()));
	}

	/**
	 * {@link #xmlTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getXmlTrace() {
		return xmlTrace;
	}

	/**
	 * {@link #xmlTrace} mutator.
	 * @param xmlTrace	The new value.
	 **/
	@XmlElement
	public void setXmlTrace(Boolean xmlTrace) {
		preset(xmlTracePropertyName, xmlTrace);
		this.xmlTrace = xmlTrace;
	}

	/**
	 * {@link #httpTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getHttpTrace() {
		return httpTrace;
	}

	/**
	 * {@link #httpTrace} mutator.
	 * @param httpTrace	The new value.
	 **/
	@XmlElement
	public void setHttpTrace(Boolean httpTrace) {
		preset(httpTracePropertyName, httpTrace);
		this.httpTrace = httpTrace;
	}

	/**
	 * {@link #queryTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getQueryTrace() {
		return queryTrace;
	}

	/**
	 * {@link #queryTrace} mutator.
	 * @param queryTrace	The new value.
	 **/
	@XmlElement
	public void setQueryTrace(Boolean queryTrace) {
		preset(queryTracePropertyName, queryTrace);
		this.queryTrace = queryTrace;
	}

	/**
	 * {@link #commandTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getCommandTrace() {
		return commandTrace;
	}

	/**
	 * {@link #commandTrace} mutator.
	 * @param commandTrace	The new value.
	 **/
	@XmlElement
	public void setCommandTrace(Boolean commandTrace) {
		preset(commandTracePropertyName, commandTrace);
		this.commandTrace = commandTrace;
	}

	/**
	 * {@link #facesTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getFacesTrace() {
		return facesTrace;
	}

	/**
	 * {@link #facesTrace} mutator.
	 * @param facesTrace	The new value.
	 **/
	@XmlElement
	public void setFacesTrace(Boolean facesTrace) {
		preset(facesTracePropertyName, facesTrace);
		this.facesTrace = facesTrace;
	}

	/**
	 * {@link #contentTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getContentTrace() {
		return contentTrace;
	}

	/**
	 * {@link #contentTrace} mutator.
	 * @param contentTrace	The new value.
	 **/
	@XmlElement
	public void setContentTrace(Boolean contentTrace) {
		preset(contentTracePropertyName, contentTrace);
		this.contentTrace = contentTrace;
	}

	/**
	 * {@link #securityTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getSecurityTrace() {
		return securityTrace;
	}

	/**
	 * {@link #securityTrace} mutator.
	 * @param securityTrace	The new value.
	 **/
	@XmlElement
	public void setSecurityTrace(Boolean securityTrace) {
		preset(securityTracePropertyName, securityTrace);
		this.securityTrace = securityTrace;
	}

	/**
	 * {@link #bizletTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getBizletTrace() {
		return bizletTrace;
	}

	/**
	 * {@link #bizletTrace} mutator.
	 * @param bizletTrace	The new value.
	 **/
	@XmlElement
	public void setBizletTrace(Boolean bizletTrace) {
		preset(bizletTracePropertyName, bizletTrace);
		this.bizletTrace = bizletTrace;
	}

	/**
	 * {@link #dirtyTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getDirtyTrace() {
		return dirtyTrace;
	}

	/**
	 * {@link #dirtyTrace} mutator.
	 * @param dirtyTrace	The new value.
	 **/
	@XmlElement
	public void setDirtyTrace(Boolean dirtyTrace) {
		preset(dirtyTracePropertyName, dirtyTrace);
		this.dirtyTrace = dirtyTrace;
	}

	/**
	 * {@link #query} accessor.
	 * @return	The value.
	 **/
	public String getQuery() {
		return query;
	}

	/**
	 * {@link #query} mutator.
	 * @param query	The new value.
	 **/
	@XmlElement
	public void setQuery(String query) {
		preset(queryPropertyName, query);
		this.query = query;
	}

	/**
	 * {@link #customerNameToSwapTo} accessor.
	 * @return	The value.
	 **/
	public String getCustomerNameToSwapTo() {
		return customerNameToSwapTo;
	}

	/**
	 * {@link #customerNameToSwapTo} mutator.
	 * @param customerNameToSwapTo	The new value.
	 **/
	@XmlElement
	public void setCustomerNameToSwapTo(String customerNameToSwapTo) {
		preset(customerNameToSwapToPropertyName, customerNameToSwapTo);
		this.customerNameToSwapTo = customerNameToSwapTo;
	}

	/**
	 * {@link #sailUser} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getSailUser() {
		return sailUser;
	}

	/**
	 * {@link #sailUser} mutator.
	 * @param sailUser	The new value.
	 **/
	@XmlElement
	public void setSailUser(UserProxyExtension sailUser) {
		if (this.sailUser != sailUser) {
			preset(sailUserPropertyName, sailUser);
			this.sailUser = sailUser;
		}
	}

	/**
	 * {@link #sailModuleName} accessor.
	 * @return	The value.
	 **/
	public String getSailModuleName() {
		return sailModuleName;
	}

	/**
	 * {@link #sailModuleName} mutator.
	 * @param sailModuleName	The new value.
	 **/
	@XmlElement
	public void setSailModuleName(String sailModuleName) {
		preset(sailModuleNamePropertyName, sailModuleName);
		this.sailModuleName = sailModuleName;
	}

	/**
	 * {@link #sailUxUi} accessor.
	 * @return	The value.
	 **/
	public String getSailUxUi() {
		return sailUxUi;
	}

	/**
	 * {@link #sailUxUi} mutator.
	 * @param sailUxUi	The new value.
	 **/
	@XmlElement
	public void setSailUxUi(String sailUxUi) {
		preset(sailUxUiPropertyName, sailUxUi);
		this.sailUxUi = sailUxUi;
	}

	/**
	 * {@link #sailUserAgentType} accessor.
	 * @return	The value.
	 **/
	public SailUserAgentType getSailUserAgentType() {
		return sailUserAgentType;
	}

	/**
	 * {@link #sailUserAgentType} mutator.
	 * @param sailUserAgentType	The new value.
	 **/
	@XmlElement
	public void setSailUserAgentType(SailUserAgentType sailUserAgentType) {
		preset(sailUserAgentTypePropertyName, sailUserAgentType);
		this.sailUserAgentType = sailUserAgentType;
	}

	/**
	 * {@link #sailTestStrategy} accessor.
	 * @return	The value.
	 **/
	public SailTestStrategy getSailTestStrategy() {
		return sailTestStrategy;
	}

	/**
	 * {@link #sailTestStrategy} mutator.
	 * @param sailTestStrategy	The new value.
	 **/
	@XmlElement
	public void setSailTestStrategy(SailTestStrategy sailTestStrategy) {
		preset(sailTestStrategyPropertyName, sailTestStrategy);
		this.sailTestStrategy = sailTestStrategy;
	}

	/**
	 * {@link #sailExecutor} accessor.
	 * @return	The value.
	 **/
	public SailExecutor getSailExecutor() {
		return sailExecutor;
	}

	/**
	 * {@link #sailExecutor} mutator.
	 * @param sailExecutor	The new value.
	 **/
	@XmlElement
	public void setSailExecutor(SailExecutor sailExecutor) {
		preset(sailExecutorPropertyName, sailExecutor);
		this.sailExecutor = sailExecutor;
	}

	/**
	 * {@link #sailComponentBuilder} accessor.
	 * @return	The value.
	 **/
	public String getSailComponentBuilder() {
		return sailComponentBuilder;
	}

	/**
	 * {@link #sailComponentBuilder} mutator.
	 * @param sailComponentBuilder	The new value.
	 **/
	@XmlElement
	public void setSailComponentBuilder(String sailComponentBuilder) {
		preset(sailComponentBuilderPropertyName, sailComponentBuilder);
		this.sailComponentBuilder = sailComponentBuilder;
	}

	/**
	 * {@link #sailLayoutBuilder} accessor.
	 * @return	The value.
	 **/
	public String getSailLayoutBuilder() {
		return sailLayoutBuilder;
	}

	/**
	 * {@link #sailLayoutBuilder} mutator.
	 * @param sailLayoutBuilder	The new value.
	 **/
	@XmlElement
	public void setSailLayoutBuilder(String sailLayoutBuilder) {
		preset(sailLayoutBuilderPropertyName, sailLayoutBuilder);
		this.sailLayoutBuilder = sailLayoutBuilder;
	}

	/**
	 * {@link #sail} accessor.
	 * @return	The value.
	 **/
	public String getSail() {
		return sail;
	}

	/**
	 * {@link #sail} mutator.
	 * @param sail	The new value.
	 **/
	@XmlElement
	public void setSail(String sail) {
		preset(sailPropertyName, sail);
		this.sail = sail;
	}

	/**
	 * {@link #sailLoginCustomer} accessor.
	 * @return	The value.
	 **/
	public String getSailLoginCustomer() {
		return sailLoginCustomer;
	}

	/**
	 * {@link #sailLoginCustomer} mutator.
	 * @param sailLoginCustomer	The new value.
	 **/
	@XmlElement
	public void setSailLoginCustomer(String sailLoginCustomer) {
		preset(sailLoginCustomerPropertyName, sailLoginCustomer);
		this.sailLoginCustomer = sailLoginCustomer;
	}

	/**
	 * {@link #sailLoginPassword} accessor.
	 * @return	The value.
	 **/
	public String getSailLoginPassword() {
		return sailLoginPassword;
	}

	/**
	 * {@link #sailLoginPassword} mutator.
	 * @param sailLoginPassword	The new value.
	 **/
	@XmlElement
	public void setSailLoginPassword(String sailLoginPassword) {
		preset(sailLoginPasswordPropertyName, sailLoginPassword);
		this.sailLoginPassword = sailLoginPassword;
	}

	/**
	 * {@link #sailBaseUrl} accessor.
	 * @return	The value.
	 **/
	public String getSailBaseUrl() {
		return sailBaseUrl;
	}

	/**
	 * {@link #sailBaseUrl} mutator.
	 * @param sailBaseUrl	The new value.
	 **/
	@XmlElement
	public void setSailBaseUrl(String sailBaseUrl) {
		preset(sailBaseUrlPropertyName, sailBaseUrl);
		this.sailBaseUrl = sailBaseUrl;
	}

	/**
	 * {@link #results} accessor.
	 * @return	The value.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * @param results	The new value.
	 **/
	@XmlElement
	public void setResults(String results) {
		preset(resultsPropertyName, results);
		this.results = results;
	}

	/**
	 * {@link #tabIndex} accessor.
	 * @return	The value.
	 **/
	public Integer getTabIndex() {
		return tabIndex;
	}

	/**
	 * {@link #tabIndex} mutator.
	 * @param tabIndex	The new value.
	 **/
	@XmlElement
	public void setTabIndex(Integer tabIndex) {
		preset(tabIndexPropertyName, tabIndex);
		this.tabIndex = tabIndex;
	}

	/**
	 * {@link #selectedCache} accessor.
	 * @return	The value.
	 **/
	public String getSelectedCache() {
		return selectedCache;
	}

	/**
	 * {@link #selectedCache} mutator.
	 * @param selectedCache	The new value.
	 **/
	@XmlElement
	public void setSelectedCache(String selectedCache) {
		this.selectedCache = selectedCache;
	}

	/**
	 * {@link #sessionCount} accessor.
	 * @return	The value.
	 **/
	public Integer getSessionCount() {
		return sessionCount;
	}

	/**
	 * {@link #sessionCount} mutator.
	 * @param sessionCount	The new value.
	 **/
	@XmlElement
	public void setSessionCount(Integer sessionCount) {
		this.sessionCount = sessionCount;
	}

	/**
	 * {@link #testNumberToGenerate} accessor.
	 * @return	The value.
	 **/
	public Integer getTestNumberToGenerate() {
		return testNumberToGenerate;
	}

	/**
	 * {@link #testNumberToGenerate} mutator.
	 * @param testNumberToGenerate	The new value.
	 **/
	@XmlElement
	public void setTestNumberToGenerate(Integer testNumberToGenerate) {
		preset(testNumberToGeneratePropertyName, testNumberToGenerate);
		this.testNumberToGenerate = testNumberToGenerate;
	}

	/**
	 * {@link #testModuleName} accessor.
	 * @return	The value.
	 **/
	public String getTestModuleName() {
		return testModuleName;
	}

	/**
	 * {@link #testModuleName} mutator.
	 * @param testModuleName	The new value.
	 **/
	@XmlElement
	public void setTestModuleName(String testModuleName) {
		preset(testModuleNamePropertyName, testModuleName);
		this.testModuleName = testModuleName;
	}

	/**
	 * {@link #testTagName} accessor.
	 * @return	The value.
	 **/
	public String getTestTagName() {
		return testTagName;
	}

	/**
	 * {@link #testTagName} mutator.
	 * @param testTagName	The new value.
	 **/
	@XmlElement
	public void setTestTagName(String testTagName) {
		preset(testTagNamePropertyName, testTagName);
		this.testTagName = testTagName;
	}

	/**
	 * {@link #testTagGeneratedData} accessor.
	 * @return	The value.
	 **/
	public Boolean getTestTagGeneratedData() {
		return testTagGeneratedData;
	}

	/**
	 * {@link #testTagGeneratedData} mutator.
	 * @param testTagGeneratedData	The new value.
	 **/
	@XmlElement
	public void setTestTagGeneratedData(Boolean testTagGeneratedData) {
		preset(testTagGeneratedDataPropertyName, testTagGeneratedData);
		this.testTagGeneratedData = testTagGeneratedData;
	}

	/**
	 * {@link #testDocumentNames} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ModuleDocument> getTestDocumentNames() {
		return testDocumentNames;
	}

	/**
	 * {@link #testDocumentNames} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ModuleDocument getTestDocumentNamesElementById(String bizId) {
		return getElementById(testDocumentNames, bizId);
	}

	/**
	 * {@link #testDocumentNames} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setTestDocumentNamesElementById(String bizId, ModuleDocument element) {
		setElementById(testDocumentNames, element);
	}

	/**
	 * {@link #testDocumentNames} add.
	 * @param element	The element to add.
	 **/
	public boolean addTestDocumentNamesElement(ModuleDocument element) {
		return testDocumentNames.add(element);
	}

	/**
	 * {@link #testDocumentNames} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addTestDocumentNamesElement(int index, ModuleDocument element) {
		testDocumentNames.add(index, element);
	}

	/**
	 * {@link #testDocumentNames} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeTestDocumentNamesElement(ModuleDocument element) {
		return testDocumentNames.remove(element);
	}

	/**
	 * {@link #testDocumentNames} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ModuleDocument removeTestDocumentNamesElement(int index) {
		return testDocumentNames.remove(index);
	}

	/**
	 * If this instance is for 1 fixed customer only.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isFixedCustomer() {
		return (org.skyve.impl.util.UtilImpl.CUSTOMER != null);
	}

	/**
	 * {@link #isFixedCustomer} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotFixedCustomer() {
		return (! isFixedCustomer());
	}

	/**
	 * productionInstance
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isProductionInstance() {
		return (org.skyve.impl.util.UtilImpl.ENVIRONMENT_IDENTIFIER==null);
	}

	/**
	 * {@link #isProductionInstance} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotProductionInstance() {
		return (! isProductionInstance());
	}

	/**
	 * resultsNotNull
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isResultsNotNull() {
		return (getResults()!=null);
	}

	/**
	 * {@link #isResultsNotNull} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotResultsNotNull() {
		return (! isResultsNotNull());
	}

	/**
	 * taggingGeneratedDataSelected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTaggingGeneratedDataSelected() {
		return (Boolean.TRUE.equals(getTestTagGeneratedData()));
	}

	/**
	 * {@link #isTaggingGeneratedDataSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTaggingGeneratedDataSelected() {
		return (! isTaggingGeneratedDataSelected());
	}
}
