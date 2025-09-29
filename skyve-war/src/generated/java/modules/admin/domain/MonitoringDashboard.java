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
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Monitoring Dashboard
 * 
 * @depend - - - RequestType
 * @depend - - - Metric
 * @depend - - - Period
 * @depend - - - Period
 * @depend - - - Period
 * @depend - - - Period
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class MonitoringDashboard extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "MonitoringDashboard";

	/** @hidden */
	public static final String topNPropertyName = "topN";

	/** @hidden */
	public static final String requestTypePropertyName = "requestType";

	/** @hidden */
	public static final String metricPropertyName = "metric";

	/** @hidden */
	public static final String periodPropertyName = "period";

	/** @hidden */
	public static final String documentStatsPeriodPropertyName = "documentStatsPeriod";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String queryStatsPeriodPropertyName = "queryStatsPeriod";

	/** @hidden */
	public static final String queryNamePropertyName = "queryName";

	/** @hidden */
	public static final String systemResourcesPeriodPropertyName = "systemResourcesPeriod";

	/**
	 * Request Type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum RequestType implements Enumeration {
		all("all", "All Request Types"),
		C("C", "Document Create"),
		E("E", "Document Edit"),
		Q("Q", "Query/List (Document List, Query List, Model List, SmartList)"),
		P("P", "Map"),
		H("H", "Chart"),
		R("R", "Content Request"),
		A("A", "PrimeFaces AJAX"),
		N("N", "Page Request"),
		U("U", "SmartEdit"),
		M("M", "SmartClient Model"),
		G("G", "SmartClient Generate"),
		L("L", "SmartClient List"),
		O("O", "Smart Complete"),
		S("S", "SmartClient Search"),
		Z("Z", "SmartClient Snap"),
		T("T", "SmartClient Tag"),
		D("D", "Dynamic Image"),
		J("J", "Report/Export"),
		B("B", "Bizport Export"),
		W("W", "Download"),
		V("V", "Customer Resource");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(RequestType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private RequestType(String code, String description) {
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

		public static RequestType fromCode(String code) {
			RequestType result = null;

			for (RequestType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static RequestType fromLocalisedDescription(String description) {
			RequestType result = null;

			for (RequestType value : values()) {
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
	 * Metric
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Metric implements Enumeration {
		elapsedTime("elapsedTime", "Elapsed Time"),
		CPUTimeDelta("CPUTimeDelta", "CPU Time Delta"),
		RAMUsageDelta("RAMUsageDelta", "RAM Usage Delta");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Metric::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Metric(String code, String description) {
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

		public static Metric fromCode(String code) {
			Metric result = null;

			for (Metric value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Metric fromLocalisedDescription(String description) {
			Metric result = null;

			for (Metric value : values()) {
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
	 * Period
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Period implements Enumeration {
		pastMinute("oneMinute", "Past Minute"),
		pastHour("oneHour", "Past Hour"),
		pastDay("oneDay", "Past Day"),
		pastWeek("oneWeek", "Past Week"),
		pastYear("oneYear", "Past Year");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Period::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Period(String code, String description) {
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

		public static Period fromCode(String code) {
			Period result = null;

			for (Period value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Period fromLocalisedDescription(String description) {
			Period result = null;

			for (Period value : values()) {
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
	 * Top
	 **/
	private Integer topN = Integer.valueOf(10);

	/**
	 * Request Type
	 **/
	private RequestType requestType = RequestType.E;

	/**
	 * Metric
	 **/
	private Metric metric = Metric.elapsedTime;

	/**
	 * Period
	 **/
	private Period period = Period.pastDay;

	/**
	 * Period
	 **/
	private Period documentStatsPeriod = Period.pastDay;

	/**
	 * Document name
	 **/
	private String documentName;

	/**
	 * Period
	 **/
	private Period queryStatsPeriod = Period.pastDay;

	/**
	 * Query name
	 **/
	private String queryName;

	/**
	 * Period
	 **/
	private Period systemResourcesPeriod = Period.pastDay;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MonitoringDashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MonitoringDashboard.DOCUMENT_NAME;
	}

	public static MonitoringDashboard newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Monitoring Dashboard", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #topN} accessor.
	 * @return	The value.
	 **/
	public Integer getTopN() {
		return topN;
	}

	/**
	 * {@link #topN} mutator.
	 * @param topN	The new value.
	 **/
	@XmlElement
	public void setTopN(Integer topN) {
		preset(topNPropertyName, topN);
		this.topN = topN;
	}

	/**
	 * {@link #requestType} accessor.
	 * @return	The value.
	 **/
	public RequestType getRequestType() {
		return requestType;
	}

	/**
	 * {@link #requestType} mutator.
	 * @param requestType	The new value.
	 **/
	@XmlElement
	public void setRequestType(RequestType requestType) {
		preset(requestTypePropertyName, requestType);
		this.requestType = requestType;
	}

	/**
	 * {@link #metric} accessor.
	 * @return	The value.
	 **/
	public Metric getMetric() {
		return metric;
	}

	/**
	 * {@link #metric} mutator.
	 * @param metric	The new value.
	 **/
	@XmlElement
	public void setMetric(Metric metric) {
		preset(metricPropertyName, metric);
		this.metric = metric;
	}

	/**
	 * {@link #period} accessor.
	 * @return	The value.
	 **/
	public Period getPeriod() {
		return period;
	}

	/**
	 * {@link #period} mutator.
	 * @param period	The new value.
	 **/
	@XmlElement
	public void setPeriod(Period period) {
		preset(periodPropertyName, period);
		this.period = period;
	}

	/**
	 * {@link #documentStatsPeriod} accessor.
	 * @return	The value.
	 **/
	public Period getDocumentStatsPeriod() {
		return documentStatsPeriod;
	}

	/**
	 * {@link #documentStatsPeriod} mutator.
	 * @param documentStatsPeriod	The new value.
	 **/
	@XmlElement
	public void setDocumentStatsPeriod(Period documentStatsPeriod) {
		preset(documentStatsPeriodPropertyName, documentStatsPeriod);
		this.documentStatsPeriod = documentStatsPeriod;
	}

	/**
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #queryStatsPeriod} accessor.
	 * @return	The value.
	 **/
	public Period getQueryStatsPeriod() {
		return queryStatsPeriod;
	}

	/**
	 * {@link #queryStatsPeriod} mutator.
	 * @param queryStatsPeriod	The new value.
	 **/
	@XmlElement
	public void setQueryStatsPeriod(Period queryStatsPeriod) {
		preset(queryStatsPeriodPropertyName, queryStatsPeriod);
		this.queryStatsPeriod = queryStatsPeriod;
	}

	/**
	 * {@link #queryName} accessor.
	 * @return	The value.
	 **/
	public String getQueryName() {
		return queryName;
	}

	/**
	 * {@link #queryName} mutator.
	 * @param queryName	The new value.
	 **/
	@XmlElement
	public void setQueryName(String queryName) {
		preset(queryNamePropertyName, queryName);
		this.queryName = queryName;
	}

	/**
	 * {@link #systemResourcesPeriod} accessor.
	 * @return	The value.
	 **/
	public Period getSystemResourcesPeriod() {
		return systemResourcesPeriod;
	}

	/**
	 * {@link #systemResourcesPeriod} mutator.
	 * @param systemResourcesPeriod	The new value.
	 **/
	@XmlElement
	public void setSystemResourcesPeriod(Period systemResourcesPeriod) {
		preset(systemResourcesPeriodPropertyName, systemResourcesPeriod);
		this.systemResourcesPeriod = systemResourcesPeriod;
	}
}
