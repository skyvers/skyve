package modules.admin.domain;

import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.Decimal10Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal2Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal5Mapper;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.impl.domain.types.jaxb.TimeOnlyMapper;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * Generic
 * <br/>
 * This document is used as the driving document for List and Tree models.
			It has a parent document of itself so that it will work in a treeGrid.
			Also there ara a bunch of generic columns to bind to.
 * 
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
public class Generic extends AbstractTransientBean implements HierarchicalBean<Generic> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Generic";

	/** @hidden */
	public static final String memo1PropertyName = "memo1";

	/** @hidden */
	public static final String memo2PropertyName = "memo2";

	/** @hidden */
	public static final String memo3PropertyName = "memo3";

	/** @hidden */
	public static final String memo4PropertyName = "memo4";

	/** @hidden */
	public static final String memo5PropertyName = "memo5";

	/** @hidden */
	public static final String memo6PropertyName = "memo6";

	/** @hidden */
	public static final String boolean1PropertyName = "boolean1";

	/** @hidden */
	public static final String boolean2PropertyName = "boolean2";

	/** @hidden */
	public static final String boolean3PropertyName = "boolean3";

	/** @hidden */
	public static final String boolean4PropertyName = "boolean4";

	/** @hidden */
	public static final String boolean5PropertyName = "boolean5";

	/** @hidden */
	public static final String date1PropertyName = "date1";

	/** @hidden */
	public static final String date2PropertyName = "date2";

	/** @hidden */
	public static final String date3PropertyName = "date3";

	/** @hidden */
	public static final String date4PropertyName = "date4";

	/** @hidden */
	public static final String date5PropertyName = "date5";

	/** @hidden */
	public static final String dateTime1PropertyName = "dateTime1";

	/** @hidden */
	public static final String dateTime2PropertyName = "dateTime2";

	/** @hidden */
	public static final String dateTime3PropertyName = "dateTime3";

	/** @hidden */
	public static final String dateTime4PropertyName = "dateTime4";

	/** @hidden */
	public static final String dateTime5PropertyName = "dateTime5";

	/** @hidden */
	public static final String time1PropertyName = "time1";

	/** @hidden */
	public static final String time2PropertyName = "time2";

	/** @hidden */
	public static final String time3PropertyName = "time3";

	/** @hidden */
	public static final String time4PropertyName = "time4";

	/** @hidden */
	public static final String time5PropertyName = "time5";

	/** @hidden */
	public static final String timestamp1PropertyName = "timestamp1";

	/** @hidden */
	public static final String timestamp2PropertyName = "timestamp2";

	/** @hidden */
	public static final String timestamp3PropertyName = "timestamp3";

	/** @hidden */
	public static final String timestamp4PropertyName = "timestamp4";

	/** @hidden */
	public static final String timestamp5PropertyName = "timestamp5";

	/** @hidden */
	public static final String decimal21PropertyName = "decimal21";

	/** @hidden */
	public static final String decimal22PropertyName = "decimal22";

	/** @hidden */
	public static final String decimal23PropertyName = "decimal23";

	/** @hidden */
	public static final String decimal24PropertyName = "decimal24";

	/** @hidden */
	public static final String decimal25PropertyName = "decimal25";

	/** @hidden */
	public static final String decimal51PropertyName = "decimal51";

	/** @hidden */
	public static final String decimal52PropertyName = "decimal52";

	/** @hidden */
	public static final String decimal53PropertyName = "decimal53";

	/** @hidden */
	public static final String decimal54PropertyName = "decimal54";

	/** @hidden */
	public static final String decimal55PropertyName = "decimal55";

	/** @hidden */
	public static final String decimal101PropertyName = "decimal101";

	/** @hidden */
	public static final String decimal102PropertyName = "decimal102";

	/** @hidden */
	public static final String decimal103PropertyName = "decimal103";

	/** @hidden */
	public static final String decimal104PropertyName = "decimal104";

	/** @hidden */
	public static final String decimal105PropertyName = "decimal105";

	/** @hidden */
	public static final String integer1PropertyName = "integer1";

	/** @hidden */
	public static final String integer2PropertyName = "integer2";

	/** @hidden */
	public static final String integer3PropertyName = "integer3";

	/** @hidden */
	public static final String integer4PropertyName = "integer4";

	/** @hidden */
	public static final String integer5PropertyName = "integer5";

	/** @hidden */
	public static final String longInteger1PropertyName = "longInteger1";

	/** @hidden */
	public static final String longInteger2PropertyName = "longInteger2";

	/** @hidden */
	public static final String longInteger3PropertyName = "longInteger3";

	/** @hidden */
	public static final String longInteger4PropertyName = "longInteger4";

	/** @hidden */
	public static final String longInteger5PropertyName = "longInteger5";

	/** @hidden */
	public static final String geometry1PropertyName = "geometry1";

	/** @hidden */
	public static final String geometry2PropertyName = "geometry2";

	/** @hidden */
	public static final String geometry3PropertyName = "geometry3";

	/** @hidden */
	public static final String geometry4PropertyName = "geometry4";

	/** @hidden */
	public static final String geometry5PropertyName = "geometry5";

	/** @hidden */
	public static final String markup1PropertyName = "markup1";

	/** @hidden */
	public static final String markup2PropertyName = "markup2";

	/** @hidden */
	public static final String markup3PropertyName = "markup3";

	/** @hidden */
	public static final String markup4PropertyName = "markup4";

	/** @hidden */
	public static final String markup5PropertyName = "markup5";

	/** @hidden */
	public static final String text5001PropertyName = "text5001";

	/** @hidden */
	public static final String text5002PropertyName = "text5002";

	/** @hidden */
	public static final String text5003PropertyName = "text5003";

	/** @hidden */
	public static final String text5004PropertyName = "text5004";

	/** @hidden */
	public static final String text5005PropertyName = "text5005";

	/**
	 * memo1
	 **/
	private String memo1;

	/**
	 * memo2
	 **/
	private String memo2;

	/**
	 * memo3
	 **/
	private String memo3;

	/**
	 * memo4
	 **/
	private String memo4;

	/**
	 * memo5
	 **/
	private String memo5;

	/**
	 * memo6
	 **/
	private String memo6;

	/**
	 * boolean1
	 **/
	private Boolean boolean1;

	/**
	 * boolean2
	 **/
	private Boolean boolean2;

	/**
	 * boolean3
	 **/
	private Boolean boolean3;

	/**
	 * boolean4
	 **/
	private Boolean boolean4;

	/**
	 * boolean5
	 **/
	private Boolean boolean5;

	/**
	 * date1
	 **/
	private DateOnly date1;

	/**
	 * date2
	 **/
	private DateOnly date2;

	/**
	 * date3
	 **/
	private DateOnly date3;

	/**
	 * date4
	 **/
	private DateOnly date4;

	/**
	 * date5
	 **/
	private DateOnly date5;

	/**
	 * dateTime1
	 **/
	private DateTime dateTime1;

	/**
	 * dateTime2
	 **/
	private DateTime dateTime2;

	/**
	 * dateTime3
	 **/
	private DateTime dateTime3;

	/**
	 * dateTime4
	 **/
	private DateTime dateTime4;

	/**
	 * dateTime5
	 **/
	private DateTime dateTime5;

	/**
	 * time1
	 **/
	private TimeOnly time1;

	/**
	 * time2
	 **/
	private TimeOnly time2;

	/**
	 * time3
	 **/
	private TimeOnly time3;

	/**
	 * time4
	 **/
	private TimeOnly time4;

	/**
	 * time5
	 **/
	private TimeOnly time5;

	/**
	 * timestamp1
	 **/
	private Timestamp timestamp1;

	/**
	 * timestamp2
	 **/
	private Timestamp timestamp2;

	/**
	 * timestamp3
	 **/
	private Timestamp timestamp3;

	/**
	 * timestamp4
	 **/
	private Timestamp timestamp4;

	/**
	 * timestamp5
	 **/
	private Timestamp timestamp5;

	/**
	 * decimal21
	 **/
	private Decimal2 decimal21;

	/**
	 * decimal22
	 **/
	private Decimal2 decimal22;

	/**
	 * decimal23
	 **/
	private Decimal2 decimal23;

	/**
	 * decimal24
	 **/
	private Decimal2 decimal24;

	/**
	 * decimal25
	 **/
	private Decimal2 decimal25;

	/**
	 * decimal51
	 **/
	private Decimal5 decimal51;

	/**
	 * decimal52
	 **/
	private Decimal5 decimal52;

	/**
	 * decimal53
	 **/
	private Decimal5 decimal53;

	/**
	 * decimal54
	 **/
	private Decimal5 decimal54;

	/**
	 * decimal55
	 **/
	private Decimal5 decimal55;

	/**
	 * decimal101
	 **/
	private Decimal10 decimal101;

	/**
	 * decimal102
	 **/
	private Decimal10 decimal102;

	/**
	 * decimal103
	 **/
	private Decimal10 decimal103;

	/**
	 * decimal104
	 **/
	private Decimal10 decimal104;

	/**
	 * decimal105
	 **/
	private Decimal10 decimal105;

	/**
	 * integer1
	 **/
	private Integer integer1;

	/**
	 * integer2
	 **/
	private Integer integer2;

	/**
	 * integer3
	 **/
	private Integer integer3;

	/**
	 * integer4
	 **/
	private Integer integer4;

	/**
	 * integer5
	 **/
	private Integer integer5;

	/**
	 * longInteger1
	 **/
	private Long longInteger1;

	/**
	 * longInteger2
	 **/
	private Long longInteger2;

	/**
	 * longInteger3
	 **/
	private Long longInteger3;

	/**
	 * longInteger4
	 **/
	private Long longInteger4;

	/**
	 * longInteger5
	 **/
	private Long longInteger5;

	/**
	 * geometry1
	 **/
	private Geometry geometry1;

	/**
	 * geometry2
	 **/
	private Geometry geometry2;

	/**
	 * geometry3
	 **/
	private Geometry geometry3;

	/**
	 * geometry4
	 **/
	private Geometry geometry4;

	/**
	 * geometry5
	 **/
	private Geometry geometry5;

	/**
	 * markup1
	 **/
	private String markup1;

	/**
	 * markup2
	 **/
	private String markup2;

	/**
	 * markup3
	 **/
	private String markup3;

	/**
	 * markup4
	 **/
	private String markup4;

	/**
	 * markup5
	 **/
	private String markup5;

	/**
	 * text 500 1
	 **/
	private String text5001;

	/**
	 * text 500 2
	 **/
	private String text5002;

	/**
	 * text 500 3
	 **/
	private String text5003;

	/**
	 * text 500 4
	 **/
	private String text5004;

	/**
	 * text 500 5
	 **/
	private String text5005;

	private String bizParentId;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Generic.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Generic.DOCUMENT_NAME;
	}

	public static Generic newInstance() {
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
		return ((o instanceof Generic) && 
					this.getBizId().equals(((Generic) o).getBizId()));
	}

	/**
	 * {@link #memo1} accessor.
	 * @return	The value.
	 **/
	public String getMemo1() {
		return memo1;
	}

	/**
	 * {@link #memo1} mutator.
	 * @param memo1	The new value.
	 **/
	@XmlElement
	public void setMemo1(String memo1) {
		preset(memo1PropertyName, memo1);
		this.memo1 = memo1;
	}

	/**
	 * {@link #memo2} accessor.
	 * @return	The value.
	 **/
	public String getMemo2() {
		return memo2;
	}

	/**
	 * {@link #memo2} mutator.
	 * @param memo2	The new value.
	 **/
	@XmlElement
	public void setMemo2(String memo2) {
		preset(memo2PropertyName, memo2);
		this.memo2 = memo2;
	}

	/**
	 * {@link #memo3} accessor.
	 * @return	The value.
	 **/
	public String getMemo3() {
		return memo3;
	}

	/**
	 * {@link #memo3} mutator.
	 * @param memo3	The new value.
	 **/
	@XmlElement
	public void setMemo3(String memo3) {
		preset(memo3PropertyName, memo3);
		this.memo3 = memo3;
	}

	/**
	 * {@link #memo4} accessor.
	 * @return	The value.
	 **/
	public String getMemo4() {
		return memo4;
	}

	/**
	 * {@link #memo4} mutator.
	 * @param memo4	The new value.
	 **/
	@XmlElement
	public void setMemo4(String memo4) {
		preset(memo4PropertyName, memo4);
		this.memo4 = memo4;
	}

	/**
	 * {@link #memo5} accessor.
	 * @return	The value.
	 **/
	public String getMemo5() {
		return memo5;
	}

	/**
	 * {@link #memo5} mutator.
	 * @param memo5	The new value.
	 **/
	@XmlElement
	public void setMemo5(String memo5) {
		preset(memo5PropertyName, memo5);
		this.memo5 = memo5;
	}

	/**
	 * {@link #memo6} accessor.
	 * @return	The value.
	 **/
	public String getMemo6() {
		return memo6;
	}

	/**
	 * {@link #memo6} mutator.
	 * @param memo6	The new value.
	 **/
	@XmlElement
	public void setMemo6(String memo6) {
		preset(memo6PropertyName, memo6);
		this.memo6 = memo6;
	}

	/**
	 * {@link #boolean1} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean1() {
		return boolean1;
	}

	/**
	 * {@link #boolean1} mutator.
	 * @param boolean1	The new value.
	 **/
	@XmlElement
	public void setBoolean1(Boolean boolean1) {
		preset(boolean1PropertyName, boolean1);
		this.boolean1 = boolean1;
	}

	/**
	 * {@link #boolean2} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean2() {
		return boolean2;
	}

	/**
	 * {@link #boolean2} mutator.
	 * @param boolean2	The new value.
	 **/
	@XmlElement
	public void setBoolean2(Boolean boolean2) {
		preset(boolean2PropertyName, boolean2);
		this.boolean2 = boolean2;
	}

	/**
	 * {@link #boolean3} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean3() {
		return boolean3;
	}

	/**
	 * {@link #boolean3} mutator.
	 * @param boolean3	The new value.
	 **/
	@XmlElement
	public void setBoolean3(Boolean boolean3) {
		preset(boolean3PropertyName, boolean3);
		this.boolean3 = boolean3;
	}

	/**
	 * {@link #boolean4} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean4() {
		return boolean4;
	}

	/**
	 * {@link #boolean4} mutator.
	 * @param boolean4	The new value.
	 **/
	@XmlElement
	public void setBoolean4(Boolean boolean4) {
		preset(boolean4PropertyName, boolean4);
		this.boolean4 = boolean4;
	}

	/**
	 * {@link #boolean5} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean5() {
		return boolean5;
	}

	/**
	 * {@link #boolean5} mutator.
	 * @param boolean5	The new value.
	 **/
	@XmlElement
	public void setBoolean5(Boolean boolean5) {
		preset(boolean5PropertyName, boolean5);
		this.boolean5 = boolean5;
	}

	/**
	 * {@link #date1} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate1() {
		return date1;
	}

	/**
	 * {@link #date1} mutator.
	 * @param date1	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate1(DateOnly date1) {
		preset(date1PropertyName, date1);
		this.date1 = date1;
	}

	/**
	 * {@link #date2} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate2() {
		return date2;
	}

	/**
	 * {@link #date2} mutator.
	 * @param date2	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate2(DateOnly date2) {
		preset(date2PropertyName, date2);
		this.date2 = date2;
	}

	/**
	 * {@link #date3} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate3() {
		return date3;
	}

	/**
	 * {@link #date3} mutator.
	 * @param date3	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate3(DateOnly date3) {
		preset(date3PropertyName, date3);
		this.date3 = date3;
	}

	/**
	 * {@link #date4} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate4() {
		return date4;
	}

	/**
	 * {@link #date4} mutator.
	 * @param date4	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate4(DateOnly date4) {
		preset(date4PropertyName, date4);
		this.date4 = date4;
	}

	/**
	 * {@link #date5} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate5() {
		return date5;
	}

	/**
	 * {@link #date5} mutator.
	 * @param date5	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate5(DateOnly date5) {
		preset(date5PropertyName, date5);
		this.date5 = date5;
	}

	/**
	 * {@link #dateTime1} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime1() {
		return dateTime1;
	}

	/**
	 * {@link #dateTime1} mutator.
	 * @param dateTime1	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime1(DateTime dateTime1) {
		preset(dateTime1PropertyName, dateTime1);
		this.dateTime1 = dateTime1;
	}

	/**
	 * {@link #dateTime2} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime2() {
		return dateTime2;
	}

	/**
	 * {@link #dateTime2} mutator.
	 * @param dateTime2	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime2(DateTime dateTime2) {
		preset(dateTime2PropertyName, dateTime2);
		this.dateTime2 = dateTime2;
	}

	/**
	 * {@link #dateTime3} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime3() {
		return dateTime3;
	}

	/**
	 * {@link #dateTime3} mutator.
	 * @param dateTime3	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime3(DateTime dateTime3) {
		preset(dateTime3PropertyName, dateTime3);
		this.dateTime3 = dateTime3;
	}

	/**
	 * {@link #dateTime4} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime4() {
		return dateTime4;
	}

	/**
	 * {@link #dateTime4} mutator.
	 * @param dateTime4	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime4(DateTime dateTime4) {
		preset(dateTime4PropertyName, dateTime4);
		this.dateTime4 = dateTime4;
	}

	/**
	 * {@link #dateTime5} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime5() {
		return dateTime5;
	}

	/**
	 * {@link #dateTime5} mutator.
	 * @param dateTime5	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime5(DateTime dateTime5) {
		preset(dateTime5PropertyName, dateTime5);
		this.dateTime5 = dateTime5;
	}

	/**
	 * {@link #time1} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime1() {
		return time1;
	}

	/**
	 * {@link #time1} mutator.
	 * @param time1	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime1(TimeOnly time1) {
		preset(time1PropertyName, time1);
		this.time1 = time1;
	}

	/**
	 * {@link #time2} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime2() {
		return time2;
	}

	/**
	 * {@link #time2} mutator.
	 * @param time2	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime2(TimeOnly time2) {
		preset(time2PropertyName, time2);
		this.time2 = time2;
	}

	/**
	 * {@link #time3} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime3() {
		return time3;
	}

	/**
	 * {@link #time3} mutator.
	 * @param time3	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime3(TimeOnly time3) {
		preset(time3PropertyName, time3);
		this.time3 = time3;
	}

	/**
	 * {@link #time4} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime4() {
		return time4;
	}

	/**
	 * {@link #time4} mutator.
	 * @param time4	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime4(TimeOnly time4) {
		preset(time4PropertyName, time4);
		this.time4 = time4;
	}

	/**
	 * {@link #time5} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime5() {
		return time5;
	}

	/**
	 * {@link #time5} mutator.
	 * @param time5	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime5(TimeOnly time5) {
		preset(time5PropertyName, time5);
		this.time5 = time5;
	}

	/**
	 * {@link #timestamp1} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp1() {
		return timestamp1;
	}

	/**
	 * {@link #timestamp1} mutator.
	 * @param timestamp1	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp1(Timestamp timestamp1) {
		preset(timestamp1PropertyName, timestamp1);
		this.timestamp1 = timestamp1;
	}

	/**
	 * {@link #timestamp2} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp2() {
		return timestamp2;
	}

	/**
	 * {@link #timestamp2} mutator.
	 * @param timestamp2	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp2(Timestamp timestamp2) {
		preset(timestamp2PropertyName, timestamp2);
		this.timestamp2 = timestamp2;
	}

	/**
	 * {@link #timestamp3} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp3() {
		return timestamp3;
	}

	/**
	 * {@link #timestamp3} mutator.
	 * @param timestamp3	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp3(Timestamp timestamp3) {
		preset(timestamp3PropertyName, timestamp3);
		this.timestamp3 = timestamp3;
	}

	/**
	 * {@link #timestamp4} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp4() {
		return timestamp4;
	}

	/**
	 * {@link #timestamp4} mutator.
	 * @param timestamp4	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp4(Timestamp timestamp4) {
		preset(timestamp4PropertyName, timestamp4);
		this.timestamp4 = timestamp4;
	}

	/**
	 * {@link #timestamp5} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp5() {
		return timestamp5;
	}

	/**
	 * {@link #timestamp5} mutator.
	 * @param timestamp5	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp5(Timestamp timestamp5) {
		preset(timestamp5PropertyName, timestamp5);
		this.timestamp5 = timestamp5;
	}

	/**
	 * {@link #decimal21} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal21() {
		return decimal21;
	}

	/**
	 * {@link #decimal21} mutator.
	 * @param decimal21	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal21(Decimal2 decimal21) {
		preset(decimal21PropertyName, decimal21);
		this.decimal21 = decimal21;
	}

	/**
	 * {@link #decimal22} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal22() {
		return decimal22;
	}

	/**
	 * {@link #decimal22} mutator.
	 * @param decimal22	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal22(Decimal2 decimal22) {
		preset(decimal22PropertyName, decimal22);
		this.decimal22 = decimal22;
	}

	/**
	 * {@link #decimal23} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal23() {
		return decimal23;
	}

	/**
	 * {@link #decimal23} mutator.
	 * @param decimal23	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal23(Decimal2 decimal23) {
		preset(decimal23PropertyName, decimal23);
		this.decimal23 = decimal23;
	}

	/**
	 * {@link #decimal24} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal24() {
		return decimal24;
	}

	/**
	 * {@link #decimal24} mutator.
	 * @param decimal24	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal24(Decimal2 decimal24) {
		preset(decimal24PropertyName, decimal24);
		this.decimal24 = decimal24;
	}

	/**
	 * {@link #decimal25} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal25() {
		return decimal25;
	}

	/**
	 * {@link #decimal25} mutator.
	 * @param decimal25	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal25(Decimal2 decimal25) {
		preset(decimal25PropertyName, decimal25);
		this.decimal25 = decimal25;
	}

	/**
	 * {@link #decimal51} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal51() {
		return decimal51;
	}

	/**
	 * {@link #decimal51} mutator.
	 * @param decimal51	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal51(Decimal5 decimal51) {
		preset(decimal51PropertyName, decimal51);
		this.decimal51 = decimal51;
	}

	/**
	 * {@link #decimal52} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal52() {
		return decimal52;
	}

	/**
	 * {@link #decimal52} mutator.
	 * @param decimal52	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal52(Decimal5 decimal52) {
		preset(decimal52PropertyName, decimal52);
		this.decimal52 = decimal52;
	}

	/**
	 * {@link #decimal53} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal53() {
		return decimal53;
	}

	/**
	 * {@link #decimal53} mutator.
	 * @param decimal53	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal53(Decimal5 decimal53) {
		preset(decimal53PropertyName, decimal53);
		this.decimal53 = decimal53;
	}

	/**
	 * {@link #decimal54} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal54() {
		return decimal54;
	}

	/**
	 * {@link #decimal54} mutator.
	 * @param decimal54	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal54(Decimal5 decimal54) {
		preset(decimal54PropertyName, decimal54);
		this.decimal54 = decimal54;
	}

	/**
	 * {@link #decimal55} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal55() {
		return decimal55;
	}

	/**
	 * {@link #decimal55} mutator.
	 * @param decimal55	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal55(Decimal5 decimal55) {
		preset(decimal55PropertyName, decimal55);
		this.decimal55 = decimal55;
	}

	/**
	 * {@link #decimal101} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal101() {
		return decimal101;
	}

	/**
	 * {@link #decimal101} mutator.
	 * @param decimal101	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal101(Decimal10 decimal101) {
		preset(decimal101PropertyName, decimal101);
		this.decimal101 = decimal101;
	}

	/**
	 * {@link #decimal102} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal102() {
		return decimal102;
	}

	/**
	 * {@link #decimal102} mutator.
	 * @param decimal102	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal102(Decimal10 decimal102) {
		preset(decimal102PropertyName, decimal102);
		this.decimal102 = decimal102;
	}

	/**
	 * {@link #decimal103} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal103() {
		return decimal103;
	}

	/**
	 * {@link #decimal103} mutator.
	 * @param decimal103	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal103(Decimal10 decimal103) {
		preset(decimal103PropertyName, decimal103);
		this.decimal103 = decimal103;
	}

	/**
	 * {@link #decimal104} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal104() {
		return decimal104;
	}

	/**
	 * {@link #decimal104} mutator.
	 * @param decimal104	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal104(Decimal10 decimal104) {
		preset(decimal104PropertyName, decimal104);
		this.decimal104 = decimal104;
	}

	/**
	 * {@link #decimal105} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal105() {
		return decimal105;
	}

	/**
	 * {@link #decimal105} mutator.
	 * @param decimal105	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal105(Decimal10 decimal105) {
		preset(decimal105PropertyName, decimal105);
		this.decimal105 = decimal105;
	}

	/**
	 * {@link #integer1} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger1() {
		return integer1;
	}

	/**
	 * {@link #integer1} mutator.
	 * @param integer1	The new value.
	 **/
	@XmlElement
	public void setInteger1(Integer integer1) {
		preset(integer1PropertyName, integer1);
		this.integer1 = integer1;
	}

	/**
	 * {@link #integer2} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger2() {
		return integer2;
	}

	/**
	 * {@link #integer2} mutator.
	 * @param integer2	The new value.
	 **/
	@XmlElement
	public void setInteger2(Integer integer2) {
		preset(integer2PropertyName, integer2);
		this.integer2 = integer2;
	}

	/**
	 * {@link #integer3} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger3() {
		return integer3;
	}

	/**
	 * {@link #integer3} mutator.
	 * @param integer3	The new value.
	 **/
	@XmlElement
	public void setInteger3(Integer integer3) {
		preset(integer3PropertyName, integer3);
		this.integer3 = integer3;
	}

	/**
	 * {@link #integer4} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger4() {
		return integer4;
	}

	/**
	 * {@link #integer4} mutator.
	 * @param integer4	The new value.
	 **/
	@XmlElement
	public void setInteger4(Integer integer4) {
		preset(integer4PropertyName, integer4);
		this.integer4 = integer4;
	}

	/**
	 * {@link #integer5} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger5() {
		return integer5;
	}

	/**
	 * {@link #integer5} mutator.
	 * @param integer5	The new value.
	 **/
	@XmlElement
	public void setInteger5(Integer integer5) {
		preset(integer5PropertyName, integer5);
		this.integer5 = integer5;
	}

	/**
	 * {@link #longInteger1} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger1() {
		return longInteger1;
	}

	/**
	 * {@link #longInteger1} mutator.
	 * @param longInteger1	The new value.
	 **/
	@XmlElement
	public void setLongInteger1(Long longInteger1) {
		preset(longInteger1PropertyName, longInteger1);
		this.longInteger1 = longInteger1;
	}

	/**
	 * {@link #longInteger2} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger2() {
		return longInteger2;
	}

	/**
	 * {@link #longInteger2} mutator.
	 * @param longInteger2	The new value.
	 **/
	@XmlElement
	public void setLongInteger2(Long longInteger2) {
		preset(longInteger2PropertyName, longInteger2);
		this.longInteger2 = longInteger2;
	}

	/**
	 * {@link #longInteger3} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger3() {
		return longInteger3;
	}

	/**
	 * {@link #longInteger3} mutator.
	 * @param longInteger3	The new value.
	 **/
	@XmlElement
	public void setLongInteger3(Long longInteger3) {
		preset(longInteger3PropertyName, longInteger3);
		this.longInteger3 = longInteger3;
	}

	/**
	 * {@link #longInteger4} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger4() {
		return longInteger4;
	}

	/**
	 * {@link #longInteger4} mutator.
	 * @param longInteger4	The new value.
	 **/
	@XmlElement
	public void setLongInteger4(Long longInteger4) {
		preset(longInteger4PropertyName, longInteger4);
		this.longInteger4 = longInteger4;
	}

	/**
	 * {@link #longInteger5} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger5() {
		return longInteger5;
	}

	/**
	 * {@link #longInteger5} mutator.
	 * @param longInteger5	The new value.
	 **/
	@XmlElement
	public void setLongInteger5(Long longInteger5) {
		preset(longInteger5PropertyName, longInteger5);
		this.longInteger5 = longInteger5;
	}

	/**
	 * {@link #geometry1} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry1() {
		return geometry1;
	}

	/**
	 * {@link #geometry1} mutator.
	 * @param geometry1	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry1(Geometry geometry1) {
		preset(geometry1PropertyName, geometry1);
		this.geometry1 = geometry1;
	}

	/**
	 * {@link #geometry2} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry2() {
		return geometry2;
	}

	/**
	 * {@link #geometry2} mutator.
	 * @param geometry2	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry2(Geometry geometry2) {
		preset(geometry2PropertyName, geometry2);
		this.geometry2 = geometry2;
	}

	/**
	 * {@link #geometry3} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry3() {
		return geometry3;
	}

	/**
	 * {@link #geometry3} mutator.
	 * @param geometry3	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry3(Geometry geometry3) {
		preset(geometry3PropertyName, geometry3);
		this.geometry3 = geometry3;
	}

	/**
	 * {@link #geometry4} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry4() {
		return geometry4;
	}

	/**
	 * {@link #geometry4} mutator.
	 * @param geometry4	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry4(Geometry geometry4) {
		preset(geometry4PropertyName, geometry4);
		this.geometry4 = geometry4;
	}

	/**
	 * {@link #geometry5} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry5() {
		return geometry5;
	}

	/**
	 * {@link #geometry5} mutator.
	 * @param geometry5	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry5(Geometry geometry5) {
		preset(geometry5PropertyName, geometry5);
		this.geometry5 = geometry5;
	}

	/**
	 * {@link #markup1} accessor.
	 * @return	The value.
	 **/
	public String getMarkup1() {
		return markup1;
	}

	/**
	 * {@link #markup1} mutator.
	 * @param markup1	The new value.
	 **/
	@XmlElement
	public void setMarkup1(String markup1) {
		preset(markup1PropertyName, markup1);
		this.markup1 = markup1;
	}

	/**
	 * {@link #markup2} accessor.
	 * @return	The value.
	 **/
	public String getMarkup2() {
		return markup2;
	}

	/**
	 * {@link #markup2} mutator.
	 * @param markup2	The new value.
	 **/
	@XmlElement
	public void setMarkup2(String markup2) {
		preset(markup2PropertyName, markup2);
		this.markup2 = markup2;
	}

	/**
	 * {@link #markup3} accessor.
	 * @return	The value.
	 **/
	public String getMarkup3() {
		return markup3;
	}

	/**
	 * {@link #markup3} mutator.
	 * @param markup3	The new value.
	 **/
	@XmlElement
	public void setMarkup3(String markup3) {
		preset(markup3PropertyName, markup3);
		this.markup3 = markup3;
	}

	/**
	 * {@link #markup4} accessor.
	 * @return	The value.
	 **/
	public String getMarkup4() {
		return markup4;
	}

	/**
	 * {@link #markup4} mutator.
	 * @param markup4	The new value.
	 **/
	@XmlElement
	public void setMarkup4(String markup4) {
		preset(markup4PropertyName, markup4);
		this.markup4 = markup4;
	}

	/**
	 * {@link #markup5} accessor.
	 * @return	The value.
	 **/
	public String getMarkup5() {
		return markup5;
	}

	/**
	 * {@link #markup5} mutator.
	 * @param markup5	The new value.
	 **/
	@XmlElement
	public void setMarkup5(String markup5) {
		preset(markup5PropertyName, markup5);
		this.markup5 = markup5;
	}

	/**
	 * {@link #text5001} accessor.
	 * @return	The value.
	 **/
	public String getText5001() {
		return text5001;
	}

	/**
	 * {@link #text5001} mutator.
	 * @param text5001	The new value.
	 **/
	@XmlElement
	public void setText5001(String text5001) {
		preset(text5001PropertyName, text5001);
		this.text5001 = text5001;
	}

	/**
	 * {@link #text5002} accessor.
	 * @return	The value.
	 **/
	public String getText5002() {
		return text5002;
	}

	/**
	 * {@link #text5002} mutator.
	 * @param text5002	The new value.
	 **/
	@XmlElement
	public void setText5002(String text5002) {
		preset(text5002PropertyName, text5002);
		this.text5002 = text5002;
	}

	/**
	 * {@link #text5003} accessor.
	 * @return	The value.
	 **/
	public String getText5003() {
		return text5003;
	}

	/**
	 * {@link #text5003} mutator.
	 * @param text5003	The new value.
	 **/
	@XmlElement
	public void setText5003(String text5003) {
		preset(text5003PropertyName, text5003);
		this.text5003 = text5003;
	}

	/**
	 * {@link #text5004} accessor.
	 * @return	The value.
	 **/
	public String getText5004() {
		return text5004;
	}

	/**
	 * {@link #text5004} mutator.
	 * @param text5004	The new value.
	 **/
	@XmlElement
	public void setText5004(String text5004) {
		preset(text5004PropertyName, text5004);
		this.text5004 = text5004;
	}

	/**
	 * {@link #text5005} accessor.
	 * @return	The value.
	 **/
	public String getText5005() {
		return text5005;
	}

	/**
	 * {@link #text5005} mutator.
	 * @param text5005	The new value.
	 **/
	@XmlElement
	public void setText5005(String text5005) {
		preset(text5005PropertyName, text5005);
		this.text5005 = text5005;
	}

	@Override
	public String getBizParentId() {
		return bizParentId;
	}

	@Override
	@XmlElement
	public void setBizParentId(String bizParentId) {
		preset(HierarchicalBean.PARENT_ID, bizParentId);
		this.bizParentId = bizParentId;
	}

	@Override
	public Generic getParent() {
		Generic result = null;

		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(Generic.MODULE_NAME, Generic.DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}

		return result;
	}

	@Override
	@XmlTransient
	public List<Generic> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Generic.MODULE_NAME, Generic.DOCUMENT_NAME);
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
