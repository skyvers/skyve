package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;

/**
 * Rerender Fixture
 * <br/>
 * Non-persistent Kitchen Sink fixture for a field whose change and blur handlers both rerender the view, as
			the Biz Hub Apps weekly timesheet's week ending date does: a date shown by a text field.

			Leaving Week Ending Date after a change runs both handlers. The server counts the rerenders it is sent for
			Week Ending Date, per fixture, outside the conversation (RerenderFixtureBizlet), so that two requests sent
			at once are counted as two even though the conversation keeps only the last one's changes. Show Rerender
			Count copies the count into Rerender Count.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class RerenderFixture extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "RerenderFixture";

	/** @hidden */
	public static final String weekEndingDatePropertyName = "weekEndingDate";

	/** @hidden */
	public static final String rerenderCountPropertyName = "rerenderCount";

	/**
	 * Week Ending Date
	 * <br/>
	 * Leaving it after a change runs its change handler and its blur handler, each of which rerenders the view.
	 **/
	private DateOnly weekEndingDate;

	/**
	 * Rerender Count
	 * <br/>
	 * The number of rerenders the server was sent for Week Ending Date, as Show Rerender Count last read it.
	 **/
	private Integer rerenderCount;

	@Override
	@XmlTransient
	public String getBizModule() {
		return RerenderFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return RerenderFixture.DOCUMENT_NAME;
	}

	public static RerenderFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Rerender Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #weekEndingDate} accessor.
	 * @return	The value.
	 **/
	public DateOnly getWeekEndingDate() {
		return weekEndingDate;
	}

	/**
	 * {@link #weekEndingDate} mutator.
	 * @param weekEndingDate	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setWeekEndingDate(DateOnly weekEndingDate) {
		preset(weekEndingDatePropertyName, weekEndingDate);
		this.weekEndingDate = weekEndingDate;
	}

	/**
	 * {@link #rerenderCount} accessor.
	 * @return	The value.
	 **/
	public Integer getRerenderCount() {
		return rerenderCount;
	}

	/**
	 * {@link #rerenderCount} mutator.
	 * @param rerenderCount	The new value.
	 **/
	@XmlElement
	public void setRerenderCount(Integer rerenderCount) {
		preset(rerenderCountPropertyName, rerenderCount);
		this.rerenderCount = rerenderCount;
	}
}
