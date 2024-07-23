package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
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
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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
	public static final String id1PropertyName = "id1";

	/** @hidden */
	public static final String id2PropertyName = "id2";

	/** @hidden */
	public static final String id3PropertyName = "id3";

	/** @hidden */
	public static final String id4PropertyName = "id4";

	/** @hidden */
	public static final String id5PropertyName = "id5";

	/** @hidden */
	public static final String id6PropertyName = "id6";

	/** @hidden */
	public static final String id7PropertyName = "id7";

	/** @hidden */
	public static final String id8PropertyName = "id8";

	/** @hidden */
	public static final String id9PropertyName = "id9";

	/** @hidden */
	public static final String id10PropertyName = "id10";

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
	public static final String memo7PropertyName = "memo7";

	/** @hidden */
	public static final String memo8PropertyName = "memo8";

	/** @hidden */
	public static final String memo9PropertyName = "memo9";

	/** @hidden */
	public static final String memo10PropertyName = "memo10";

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
	public static final String boolean6PropertyName = "boolean6";

	/** @hidden */
	public static final String boolean7PropertyName = "boolean7";

	/** @hidden */
	public static final String boolean8PropertyName = "boolean8";

	/** @hidden */
	public static final String boolean9PropertyName = "boolean9";

	/** @hidden */
	public static final String boolean10PropertyName = "boolean10";

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
	public static final String date6PropertyName = "date6";

	/** @hidden */
	public static final String date7PropertyName = "date7";

	/** @hidden */
	public static final String date8PropertyName = "date8";

	/** @hidden */
	public static final String date9PropertyName = "date9";

	/** @hidden */
	public static final String date10PropertyName = "date10";

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
	public static final String dateTime6PropertyName = "dateTime6";

	/** @hidden */
	public static final String dateTime7PropertyName = "dateTime7";

	/** @hidden */
	public static final String dateTime8PropertyName = "dateTime8";

	/** @hidden */
	public static final String dateTime9PropertyName = "dateTime9";

	/** @hidden */
	public static final String dateTime10PropertyName = "dateTime10";

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
	public static final String time6PropertyName = "time6";

	/** @hidden */
	public static final String time7PropertyName = "time7";

	/** @hidden */
	public static final String time8PropertyName = "time8";

	/** @hidden */
	public static final String time9PropertyName = "time9";

	/** @hidden */
	public static final String time10PropertyName = "time10";

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
	public static final String timestamp6PropertyName = "timestamp6";

	/** @hidden */
	public static final String timestamp7PropertyName = "timestamp7";

	/** @hidden */
	public static final String timestamp8PropertyName = "timestamp8";

	/** @hidden */
	public static final String timestamp9PropertyName = "timestamp9";

	/** @hidden */
	public static final String timestamp10PropertyName = "timestamp10";

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
	public static final String decimal26PropertyName = "decimal26";

	/** @hidden */
	public static final String decimal27PropertyName = "decimal27";

	/** @hidden */
	public static final String decimal28PropertyName = "decimal28";

	/** @hidden */
	public static final String decimal29PropertyName = "decimal29";

	/** @hidden */
	public static final String decimal210PropertyName = "decimal210";

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
	public static final String decimal56PropertyName = "decimal56";

	/** @hidden */
	public static final String decimal57PropertyName = "decimal57";

	/** @hidden */
	public static final String decimal58PropertyName = "decimal58";

	/** @hidden */
	public static final String decimal59PropertyName = "decimal59";

	/** @hidden */
	public static final String decimal510PropertyName = "decimal510";

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
	public static final String decimal106PropertyName = "decimal106";

	/** @hidden */
	public static final String decimal107PropertyName = "decimal107";

	/** @hidden */
	public static final String decimal108PropertyName = "decimal108";

	/** @hidden */
	public static final String decimal109PropertyName = "decimal109";

	/** @hidden */
	public static final String decimal1010PropertyName = "decimal1010";

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
	public static final String integer6PropertyName = "integer6";

	/** @hidden */
	public static final String integer7PropertyName = "integer7";

	/** @hidden */
	public static final String integer8PropertyName = "integer8";

	/** @hidden */
	public static final String integer9PropertyName = "integer9";

	/** @hidden */
	public static final String integer10PropertyName = "integer10";

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
	public static final String longInteger6PropertyName = "longInteger6";

	/** @hidden */
	public static final String longInteger7PropertyName = "longInteger7";

	/** @hidden */
	public static final String longInteger8PropertyName = "longInteger8";

	/** @hidden */
	public static final String longInteger9PropertyName = "longInteger9";

	/** @hidden */
	public static final String longInteger10PropertyName = "longInteger10";

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
	public static final String geometry6PropertyName = "geometry6";

	/** @hidden */
	public static final String geometry7PropertyName = "geometry7";

	/** @hidden */
	public static final String geometry8PropertyName = "geometry8";

	/** @hidden */
	public static final String geometry9PropertyName = "geometry9";

	/** @hidden */
	public static final String geometry10PropertyName = "geometry10";

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
	public static final String markup6PropertyName = "markup6";

	/** @hidden */
	public static final String markup7PropertyName = "markup7";

	/** @hidden */
	public static final String markup8PropertyName = "markup8";

	/** @hidden */
	public static final String markup9PropertyName = "markup9";

	/** @hidden */
	public static final String markup10PropertyName = "markup10";

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

	/** @hidden */
	public static final String text5006PropertyName = "text5006";

	/** @hidden */
	public static final String text5007PropertyName = "text5007";

	/** @hidden */
	public static final String text5008PropertyName = "text5008";

	/** @hidden */
	public static final String text5009PropertyName = "text5009";

	/** @hidden */
	public static final String text50010PropertyName = "text50010";

	/**
	 * id1
	 **/
	private String id1;

	/**
	 * id2
	 **/
	private String id2;

	/**
	 * id3
	 **/
	private String id3;

	/**
	 * id4
	 **/
	private String id4;

	/**
	 * id5
	 **/
	private String id5;

	/**
	 * id6
	 **/
	private String id6;

	/**
	 * id7
	 **/
	private String id7;

	/**
	 * id8
	 **/
	private String id8;

	/**
	 * id9
	 **/
	private String id9;

	/**
	 * id10
	 **/
	private String id10;

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
	 * memo7
	 **/
	private String memo7;

	/**
	 * memo8
	 **/
	private String memo8;

	/**
	 * memo9
	 **/
	private String memo9;

	/**
	 * memo10
	 **/
	private String memo10;

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
	 * boolean6
	 **/
	private Boolean boolean6;

	/**
	 * boolean7
	 **/
	private Boolean boolean7;

	/**
	 * boolean8
	 **/
	private Boolean boolean8;

	/**
	 * boolean9
	 **/
	private Boolean boolean9;

	/**
	 * boolean10
	 **/
	private Boolean boolean10;

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
	 * date6
	 **/
	private DateOnly date6;

	/**
	 * date7
	 **/
	private DateOnly date7;

	/**
	 * date8
	 **/
	private DateOnly date8;

	/**
	 * date9
	 **/
	private DateOnly date9;

	/**
	 * date10
	 **/
	private DateOnly date10;

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
	 * dateTime6
	 **/
	private DateTime dateTime6;

	/**
	 * dateTime7
	 **/
	private DateTime dateTime7;

	/**
	 * dateTime8
	 **/
	private DateTime dateTime8;

	/**
	 * dateTime9
	 **/
	private DateTime dateTime9;

	/**
	 * dateTime10
	 **/
	private DateTime dateTime10;

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
	 * time6
	 **/
	private TimeOnly time6;

	/**
	 * time7
	 **/
	private TimeOnly time7;

	/**
	 * time8
	 **/
	private TimeOnly time8;

	/**
	 * time9
	 **/
	private TimeOnly time9;

	/**
	 * time10
	 **/
	private TimeOnly time10;

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
	 * timestamp6
	 **/
	private Timestamp timestamp6;

	/**
	 * timestamp7
	 **/
	private Timestamp timestamp7;

	/**
	 * timestamp8
	 **/
	private Timestamp timestamp8;

	/**
	 * timestamp9
	 **/
	private Timestamp timestamp9;

	/**
	 * timestamp10
	 **/
	private Timestamp timestamp10;

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
	 * decimal26
	 **/
	private Decimal2 decimal26;

	/**
	 * decimal27
	 **/
	private Decimal2 decimal27;

	/**
	 * decimal28
	 **/
	private Decimal2 decimal28;

	/**
	 * decimal29
	 **/
	private Decimal2 decimal29;

	/**
	 * decimal210
	 **/
	private Decimal2 decimal210;

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
	 * decimal56
	 **/
	private Decimal5 decimal56;

	/**
	 * decimal57
	 **/
	private Decimal5 decimal57;

	/**
	 * decimal58
	 **/
	private Decimal5 decimal58;

	/**
	 * decimal59
	 **/
	private Decimal5 decimal59;

	/**
	 * decimal510
	 **/
	private Decimal5 decimal510;

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
	 * decimal106
	 **/
	private Decimal10 decimal106;

	/**
	 * decimal107
	 **/
	private Decimal10 decimal107;

	/**
	 * decimal108
	 **/
	private Decimal10 decimal108;

	/**
	 * decimal109
	 **/
	private Decimal10 decimal109;

	/**
	 * decimal1010
	 **/
	private Decimal10 decimal1010;

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
	 * integer6
	 **/
	private Integer integer6;

	/**
	 * integer7
	 **/
	private Integer integer7;

	/**
	 * integer8
	 **/
	private Integer integer8;

	/**
	 * integer9
	 **/
	private Integer integer9;

	/**
	 * integer10
	 **/
	private Integer integer10;

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
	 * longInteger6
	 **/
	private Long longInteger6;

	/**
	 * longInteger7
	 **/
	private Long longInteger7;

	/**
	 * longInteger8
	 **/
	private Long longInteger8;

	/**
	 * longInteger9
	 **/
	private Long longInteger9;

	/**
	 * longInteger10
	 **/
	private Long longInteger10;

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
	 * geometry6
	 **/
	private Geometry geometry6;

	/**
	 * geometry7
	 **/
	private Geometry geometry7;

	/**
	 * geometry8
	 **/
	private Geometry geometry8;

	/**
	 * geometry9
	 **/
	private Geometry geometry9;

	/**
	 * geometry10
	 **/
	private Geometry geometry10;

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
	 * markup6
	 **/
	private String markup6;

	/**
	 * markup7
	 **/
	private String markup7;

	/**
	 * markup8
	 **/
	private String markup8;

	/**
	 * markup9
	 **/
	private String markup9;

	/**
	 * markup10
	 **/
	private String markup10;

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

	/**
	 * text 500 6
	 **/
	private String text5006;

	/**
	 * text 500 7
	 **/
	private String text5007;

	/**
	 * text 500 8
	 **/
	private String text5008;

	/**
	 * text 500 9
	 **/
	private String text5009;

	/**
	 * text 500 10
	 **/
	private String text50010;

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
	 * {@link #id1} accessor.
	 * @return	The value.
	 **/
	public String getId1() {
		return id1;
	}

	/**
	 * {@link #id1} mutator.
	 * @param id1	The new value.
	 **/
	@XmlElement
	public void setId1(String id1) {
		preset(id1PropertyName, id1);
		this.id1 = id1;
	}

	/**
	 * {@link #id2} accessor.
	 * @return	The value.
	 **/
	public String getId2() {
		return id2;
	}

	/**
	 * {@link #id2} mutator.
	 * @param id2	The new value.
	 **/
	@XmlElement
	public void setId2(String id2) {
		preset(id2PropertyName, id2);
		this.id2 = id2;
	}

	/**
	 * {@link #id3} accessor.
	 * @return	The value.
	 **/
	public String getId3() {
		return id3;
	}

	/**
	 * {@link #id3} mutator.
	 * @param id3	The new value.
	 **/
	@XmlElement
	public void setId3(String id3) {
		preset(id3PropertyName, id3);
		this.id3 = id3;
	}

	/**
	 * {@link #id4} accessor.
	 * @return	The value.
	 **/
	public String getId4() {
		return id4;
	}

	/**
	 * {@link #id4} mutator.
	 * @param id4	The new value.
	 **/
	@XmlElement
	public void setId4(String id4) {
		preset(id4PropertyName, id4);
		this.id4 = id4;
	}

	/**
	 * {@link #id5} accessor.
	 * @return	The value.
	 **/
	public String getId5() {
		return id5;
	}

	/**
	 * {@link #id5} mutator.
	 * @param id5	The new value.
	 **/
	@XmlElement
	public void setId5(String id5) {
		preset(id5PropertyName, id5);
		this.id5 = id5;
	}

	/**
	 * {@link #id6} accessor.
	 * @return	The value.
	 **/
	public String getId6() {
		return id6;
	}

	/**
	 * {@link #id6} mutator.
	 * @param id6	The new value.
	 **/
	@XmlElement
	public void setId6(String id6) {
		preset(id6PropertyName, id6);
		this.id6 = id6;
	}

	/**
	 * {@link #id7} accessor.
	 * @return	The value.
	 **/
	public String getId7() {
		return id7;
	}

	/**
	 * {@link #id7} mutator.
	 * @param id7	The new value.
	 **/
	@XmlElement
	public void setId7(String id7) {
		preset(id7PropertyName, id7);
		this.id7 = id7;
	}

	/**
	 * {@link #id8} accessor.
	 * @return	The value.
	 **/
	public String getId8() {
		return id8;
	}

	/**
	 * {@link #id8} mutator.
	 * @param id8	The new value.
	 **/
	@XmlElement
	public void setId8(String id8) {
		preset(id8PropertyName, id8);
		this.id8 = id8;
	}

	/**
	 * {@link #id9} accessor.
	 * @return	The value.
	 **/
	public String getId9() {
		return id9;
	}

	/**
	 * {@link #id9} mutator.
	 * @param id9	The new value.
	 **/
	@XmlElement
	public void setId9(String id9) {
		preset(id9PropertyName, id9);
		this.id9 = id9;
	}

	/**
	 * {@link #id10} accessor.
	 * @return	The value.
	 **/
	public String getId10() {
		return id10;
	}

	/**
	 * {@link #id10} mutator.
	 * @param id10	The new value.
	 **/
	@XmlElement
	public void setId10(String id10) {
		preset(id10PropertyName, id10);
		this.id10 = id10;
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
	 * {@link #memo7} accessor.
	 * @return	The value.
	 **/
	public String getMemo7() {
		return memo7;
	}

	/**
	 * {@link #memo7} mutator.
	 * @param memo7	The new value.
	 **/
	@XmlElement
	public void setMemo7(String memo7) {
		preset(memo7PropertyName, memo7);
		this.memo7 = memo7;
	}

	/**
	 * {@link #memo8} accessor.
	 * @return	The value.
	 **/
	public String getMemo8() {
		return memo8;
	}

	/**
	 * {@link #memo8} mutator.
	 * @param memo8	The new value.
	 **/
	@XmlElement
	public void setMemo8(String memo8) {
		preset(memo8PropertyName, memo8);
		this.memo8 = memo8;
	}

	/**
	 * {@link #memo9} accessor.
	 * @return	The value.
	 **/
	public String getMemo9() {
		return memo9;
	}

	/**
	 * {@link #memo9} mutator.
	 * @param memo9	The new value.
	 **/
	@XmlElement
	public void setMemo9(String memo9) {
		preset(memo9PropertyName, memo9);
		this.memo9 = memo9;
	}

	/**
	 * {@link #memo10} accessor.
	 * @return	The value.
	 **/
	public String getMemo10() {
		return memo10;
	}

	/**
	 * {@link #memo10} mutator.
	 * @param memo10	The new value.
	 **/
	@XmlElement
	public void setMemo10(String memo10) {
		preset(memo10PropertyName, memo10);
		this.memo10 = memo10;
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
	 * {@link #boolean6} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean6() {
		return boolean6;
	}

	/**
	 * {@link #boolean6} mutator.
	 * @param boolean6	The new value.
	 **/
	@XmlElement
	public void setBoolean6(Boolean boolean6) {
		preset(boolean6PropertyName, boolean6);
		this.boolean6 = boolean6;
	}

	/**
	 * {@link #boolean7} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean7() {
		return boolean7;
	}

	/**
	 * {@link #boolean7} mutator.
	 * @param boolean7	The new value.
	 **/
	@XmlElement
	public void setBoolean7(Boolean boolean7) {
		preset(boolean7PropertyName, boolean7);
		this.boolean7 = boolean7;
	}

	/**
	 * {@link #boolean8} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean8() {
		return boolean8;
	}

	/**
	 * {@link #boolean8} mutator.
	 * @param boolean8	The new value.
	 **/
	@XmlElement
	public void setBoolean8(Boolean boolean8) {
		preset(boolean8PropertyName, boolean8);
		this.boolean8 = boolean8;
	}

	/**
	 * {@link #boolean9} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean9() {
		return boolean9;
	}

	/**
	 * {@link #boolean9} mutator.
	 * @param boolean9	The new value.
	 **/
	@XmlElement
	public void setBoolean9(Boolean boolean9) {
		preset(boolean9PropertyName, boolean9);
		this.boolean9 = boolean9;
	}

	/**
	 * {@link #boolean10} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoolean10() {
		return boolean10;
	}

	/**
	 * {@link #boolean10} mutator.
	 * @param boolean10	The new value.
	 **/
	@XmlElement
	public void setBoolean10(Boolean boolean10) {
		preset(boolean10PropertyName, boolean10);
		this.boolean10 = boolean10;
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
	 * {@link #date6} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate6() {
		return date6;
	}

	/**
	 * {@link #date6} mutator.
	 * @param date6	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate6(DateOnly date6) {
		preset(date6PropertyName, date6);
		this.date6 = date6;
	}

	/**
	 * {@link #date7} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate7() {
		return date7;
	}

	/**
	 * {@link #date7} mutator.
	 * @param date7	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate7(DateOnly date7) {
		preset(date7PropertyName, date7);
		this.date7 = date7;
	}

	/**
	 * {@link #date8} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate8() {
		return date8;
	}

	/**
	 * {@link #date8} mutator.
	 * @param date8	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate8(DateOnly date8) {
		preset(date8PropertyName, date8);
		this.date8 = date8;
	}

	/**
	 * {@link #date9} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate9() {
		return date9;
	}

	/**
	 * {@link #date9} mutator.
	 * @param date9	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate9(DateOnly date9) {
		preset(date9PropertyName, date9);
		this.date9 = date9;
	}

	/**
	 * {@link #date10} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate10() {
		return date10;
	}

	/**
	 * {@link #date10} mutator.
	 * @param date10	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate10(DateOnly date10) {
		preset(date10PropertyName, date10);
		this.date10 = date10;
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
	 * {@link #dateTime6} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime6() {
		return dateTime6;
	}

	/**
	 * {@link #dateTime6} mutator.
	 * @param dateTime6	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime6(DateTime dateTime6) {
		preset(dateTime6PropertyName, dateTime6);
		this.dateTime6 = dateTime6;
	}

	/**
	 * {@link #dateTime7} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime7() {
		return dateTime7;
	}

	/**
	 * {@link #dateTime7} mutator.
	 * @param dateTime7	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime7(DateTime dateTime7) {
		preset(dateTime7PropertyName, dateTime7);
		this.dateTime7 = dateTime7;
	}

	/**
	 * {@link #dateTime8} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime8() {
		return dateTime8;
	}

	/**
	 * {@link #dateTime8} mutator.
	 * @param dateTime8	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime8(DateTime dateTime8) {
		preset(dateTime8PropertyName, dateTime8);
		this.dateTime8 = dateTime8;
	}

	/**
	 * {@link #dateTime9} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime9() {
		return dateTime9;
	}

	/**
	 * {@link #dateTime9} mutator.
	 * @param dateTime9	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime9(DateTime dateTime9) {
		preset(dateTime9PropertyName, dateTime9);
		this.dateTime9 = dateTime9;
	}

	/**
	 * {@link #dateTime10} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime10() {
		return dateTime10;
	}

	/**
	 * {@link #dateTime10} mutator.
	 * @param dateTime10	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime10(DateTime dateTime10) {
		preset(dateTime10PropertyName, dateTime10);
		this.dateTime10 = dateTime10;
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
	 * {@link #time6} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime6() {
		return time6;
	}

	/**
	 * {@link #time6} mutator.
	 * @param time6	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime6(TimeOnly time6) {
		preset(time6PropertyName, time6);
		this.time6 = time6;
	}

	/**
	 * {@link #time7} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime7() {
		return time7;
	}

	/**
	 * {@link #time7} mutator.
	 * @param time7	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime7(TimeOnly time7) {
		preset(time7PropertyName, time7);
		this.time7 = time7;
	}

	/**
	 * {@link #time8} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime8() {
		return time8;
	}

	/**
	 * {@link #time8} mutator.
	 * @param time8	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime8(TimeOnly time8) {
		preset(time8PropertyName, time8);
		this.time8 = time8;
	}

	/**
	 * {@link #time9} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime9() {
		return time9;
	}

	/**
	 * {@link #time9} mutator.
	 * @param time9	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime9(TimeOnly time9) {
		preset(time9PropertyName, time9);
		this.time9 = time9;
	}

	/**
	 * {@link #time10} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime10() {
		return time10;
	}

	/**
	 * {@link #time10} mutator.
	 * @param time10	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime10(TimeOnly time10) {
		preset(time10PropertyName, time10);
		this.time10 = time10;
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
	 * {@link #timestamp6} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp6() {
		return timestamp6;
	}

	/**
	 * {@link #timestamp6} mutator.
	 * @param timestamp6	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp6(Timestamp timestamp6) {
		preset(timestamp6PropertyName, timestamp6);
		this.timestamp6 = timestamp6;
	}

	/**
	 * {@link #timestamp7} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp7() {
		return timestamp7;
	}

	/**
	 * {@link #timestamp7} mutator.
	 * @param timestamp7	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp7(Timestamp timestamp7) {
		preset(timestamp7PropertyName, timestamp7);
		this.timestamp7 = timestamp7;
	}

	/**
	 * {@link #timestamp8} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp8() {
		return timestamp8;
	}

	/**
	 * {@link #timestamp8} mutator.
	 * @param timestamp8	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp8(Timestamp timestamp8) {
		preset(timestamp8PropertyName, timestamp8);
		this.timestamp8 = timestamp8;
	}

	/**
	 * {@link #timestamp9} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp9() {
		return timestamp9;
	}

	/**
	 * {@link #timestamp9} mutator.
	 * @param timestamp9	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp9(Timestamp timestamp9) {
		preset(timestamp9PropertyName, timestamp9);
		this.timestamp9 = timestamp9;
	}

	/**
	 * {@link #timestamp10} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp10() {
		return timestamp10;
	}

	/**
	 * {@link #timestamp10} mutator.
	 * @param timestamp10	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp10(Timestamp timestamp10) {
		preset(timestamp10PropertyName, timestamp10);
		this.timestamp10 = timestamp10;
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
	 * {@link #decimal26} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal26() {
		return decimal26;
	}

	/**
	 * {@link #decimal26} mutator.
	 * @param decimal26	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal26(Decimal2 decimal26) {
		preset(decimal26PropertyName, decimal26);
		this.decimal26 = decimal26;
	}

	/**
	 * {@link #decimal27} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal27() {
		return decimal27;
	}

	/**
	 * {@link #decimal27} mutator.
	 * @param decimal27	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal27(Decimal2 decimal27) {
		preset(decimal27PropertyName, decimal27);
		this.decimal27 = decimal27;
	}

	/**
	 * {@link #decimal28} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal28() {
		return decimal28;
	}

	/**
	 * {@link #decimal28} mutator.
	 * @param decimal28	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal28(Decimal2 decimal28) {
		preset(decimal28PropertyName, decimal28);
		this.decimal28 = decimal28;
	}

	/**
	 * {@link #decimal29} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal29() {
		return decimal29;
	}

	/**
	 * {@link #decimal29} mutator.
	 * @param decimal29	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal29(Decimal2 decimal29) {
		preset(decimal29PropertyName, decimal29);
		this.decimal29 = decimal29;
	}

	/**
	 * {@link #decimal210} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal210() {
		return decimal210;
	}

	/**
	 * {@link #decimal210} mutator.
	 * @param decimal210	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal210(Decimal2 decimal210) {
		preset(decimal210PropertyName, decimal210);
		this.decimal210 = decimal210;
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
	 * {@link #decimal56} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal56() {
		return decimal56;
	}

	/**
	 * {@link #decimal56} mutator.
	 * @param decimal56	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal56(Decimal5 decimal56) {
		preset(decimal56PropertyName, decimal56);
		this.decimal56 = decimal56;
	}

	/**
	 * {@link #decimal57} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal57() {
		return decimal57;
	}

	/**
	 * {@link #decimal57} mutator.
	 * @param decimal57	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal57(Decimal5 decimal57) {
		preset(decimal57PropertyName, decimal57);
		this.decimal57 = decimal57;
	}

	/**
	 * {@link #decimal58} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal58() {
		return decimal58;
	}

	/**
	 * {@link #decimal58} mutator.
	 * @param decimal58	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal58(Decimal5 decimal58) {
		preset(decimal58PropertyName, decimal58);
		this.decimal58 = decimal58;
	}

	/**
	 * {@link #decimal59} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal59() {
		return decimal59;
	}

	/**
	 * {@link #decimal59} mutator.
	 * @param decimal59	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal59(Decimal5 decimal59) {
		preset(decimal59PropertyName, decimal59);
		this.decimal59 = decimal59;
	}

	/**
	 * {@link #decimal510} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal510() {
		return decimal510;
	}

	/**
	 * {@link #decimal510} mutator.
	 * @param decimal510	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal510(Decimal5 decimal510) {
		preset(decimal510PropertyName, decimal510);
		this.decimal510 = decimal510;
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
	 * {@link #decimal106} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal106() {
		return decimal106;
	}

	/**
	 * {@link #decimal106} mutator.
	 * @param decimal106	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal106(Decimal10 decimal106) {
		preset(decimal106PropertyName, decimal106);
		this.decimal106 = decimal106;
	}

	/**
	 * {@link #decimal107} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal107() {
		return decimal107;
	}

	/**
	 * {@link #decimal107} mutator.
	 * @param decimal107	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal107(Decimal10 decimal107) {
		preset(decimal107PropertyName, decimal107);
		this.decimal107 = decimal107;
	}

	/**
	 * {@link #decimal108} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal108() {
		return decimal108;
	}

	/**
	 * {@link #decimal108} mutator.
	 * @param decimal108	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal108(Decimal10 decimal108) {
		preset(decimal108PropertyName, decimal108);
		this.decimal108 = decimal108;
	}

	/**
	 * {@link #decimal109} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal109() {
		return decimal109;
	}

	/**
	 * {@link #decimal109} mutator.
	 * @param decimal109	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal109(Decimal10 decimal109) {
		preset(decimal109PropertyName, decimal109);
		this.decimal109 = decimal109;
	}

	/**
	 * {@link #decimal1010} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal1010() {
		return decimal1010;
	}

	/**
	 * {@link #decimal1010} mutator.
	 * @param decimal1010	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal1010(Decimal10 decimal1010) {
		preset(decimal1010PropertyName, decimal1010);
		this.decimal1010 = decimal1010;
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
	 * {@link #integer6} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger6() {
		return integer6;
	}

	/**
	 * {@link #integer6} mutator.
	 * @param integer6	The new value.
	 **/
	@XmlElement
	public void setInteger6(Integer integer6) {
		preset(integer6PropertyName, integer6);
		this.integer6 = integer6;
	}

	/**
	 * {@link #integer7} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger7() {
		return integer7;
	}

	/**
	 * {@link #integer7} mutator.
	 * @param integer7	The new value.
	 **/
	@XmlElement
	public void setInteger7(Integer integer7) {
		preset(integer7PropertyName, integer7);
		this.integer7 = integer7;
	}

	/**
	 * {@link #integer8} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger8() {
		return integer8;
	}

	/**
	 * {@link #integer8} mutator.
	 * @param integer8	The new value.
	 **/
	@XmlElement
	public void setInteger8(Integer integer8) {
		preset(integer8PropertyName, integer8);
		this.integer8 = integer8;
	}

	/**
	 * {@link #integer9} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger9() {
		return integer9;
	}

	/**
	 * {@link #integer9} mutator.
	 * @param integer9	The new value.
	 **/
	@XmlElement
	public void setInteger9(Integer integer9) {
		preset(integer9PropertyName, integer9);
		this.integer9 = integer9;
	}

	/**
	 * {@link #integer10} accessor.
	 * @return	The value.
	 **/
	public Integer getInteger10() {
		return integer10;
	}

	/**
	 * {@link #integer10} mutator.
	 * @param integer10	The new value.
	 **/
	@XmlElement
	public void setInteger10(Integer integer10) {
		preset(integer10PropertyName, integer10);
		this.integer10 = integer10;
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
	 * {@link #longInteger6} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger6() {
		return longInteger6;
	}

	/**
	 * {@link #longInteger6} mutator.
	 * @param longInteger6	The new value.
	 **/
	@XmlElement
	public void setLongInteger6(Long longInteger6) {
		preset(longInteger6PropertyName, longInteger6);
		this.longInteger6 = longInteger6;
	}

	/**
	 * {@link #longInteger7} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger7() {
		return longInteger7;
	}

	/**
	 * {@link #longInteger7} mutator.
	 * @param longInteger7	The new value.
	 **/
	@XmlElement
	public void setLongInteger7(Long longInteger7) {
		preset(longInteger7PropertyName, longInteger7);
		this.longInteger7 = longInteger7;
	}

	/**
	 * {@link #longInteger8} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger8() {
		return longInteger8;
	}

	/**
	 * {@link #longInteger8} mutator.
	 * @param longInteger8	The new value.
	 **/
	@XmlElement
	public void setLongInteger8(Long longInteger8) {
		preset(longInteger8PropertyName, longInteger8);
		this.longInteger8 = longInteger8;
	}

	/**
	 * {@link #longInteger9} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger9() {
		return longInteger9;
	}

	/**
	 * {@link #longInteger9} mutator.
	 * @param longInteger9	The new value.
	 **/
	@XmlElement
	public void setLongInteger9(Long longInteger9) {
		preset(longInteger9PropertyName, longInteger9);
		this.longInteger9 = longInteger9;
	}

	/**
	 * {@link #longInteger10} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger10() {
		return longInteger10;
	}

	/**
	 * {@link #longInteger10} mutator.
	 * @param longInteger10	The new value.
	 **/
	@XmlElement
	public void setLongInteger10(Long longInteger10) {
		preset(longInteger10PropertyName, longInteger10);
		this.longInteger10 = longInteger10;
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
	 * {@link #geometry6} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry6() {
		return geometry6;
	}

	/**
	 * {@link #geometry6} mutator.
	 * @param geometry6	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry6(Geometry geometry6) {
		preset(geometry6PropertyName, geometry6);
		this.geometry6 = geometry6;
	}

	/**
	 * {@link #geometry7} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry7() {
		return geometry7;
	}

	/**
	 * {@link #geometry7} mutator.
	 * @param geometry7	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry7(Geometry geometry7) {
		preset(geometry7PropertyName, geometry7);
		this.geometry7 = geometry7;
	}

	/**
	 * {@link #geometry8} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry8() {
		return geometry8;
	}

	/**
	 * {@link #geometry8} mutator.
	 * @param geometry8	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry8(Geometry geometry8) {
		preset(geometry8PropertyName, geometry8);
		this.geometry8 = geometry8;
	}

	/**
	 * {@link #geometry9} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry9() {
		return geometry9;
	}

	/**
	 * {@link #geometry9} mutator.
	 * @param geometry9	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry9(Geometry geometry9) {
		preset(geometry9PropertyName, geometry9);
		this.geometry9 = geometry9;
	}

	/**
	 * {@link #geometry10} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry10() {
		return geometry10;
	}

	/**
	 * {@link #geometry10} mutator.
	 * @param geometry10	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry10(Geometry geometry10) {
		preset(geometry10PropertyName, geometry10);
		this.geometry10 = geometry10;
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
	 * {@link #markup6} accessor.
	 * @return	The value.
	 **/
	public String getMarkup6() {
		return markup6;
	}

	/**
	 * {@link #markup6} mutator.
	 * @param markup6	The new value.
	 **/
	@XmlElement
	public void setMarkup6(String markup6) {
		preset(markup6PropertyName, markup6);
		this.markup6 = markup6;
	}

	/**
	 * {@link #markup7} accessor.
	 * @return	The value.
	 **/
	public String getMarkup7() {
		return markup7;
	}

	/**
	 * {@link #markup7} mutator.
	 * @param markup7	The new value.
	 **/
	@XmlElement
	public void setMarkup7(String markup7) {
		preset(markup7PropertyName, markup7);
		this.markup7 = markup7;
	}

	/**
	 * {@link #markup8} accessor.
	 * @return	The value.
	 **/
	public String getMarkup8() {
		return markup8;
	}

	/**
	 * {@link #markup8} mutator.
	 * @param markup8	The new value.
	 **/
	@XmlElement
	public void setMarkup8(String markup8) {
		preset(markup8PropertyName, markup8);
		this.markup8 = markup8;
	}

	/**
	 * {@link #markup9} accessor.
	 * @return	The value.
	 **/
	public String getMarkup9() {
		return markup9;
	}

	/**
	 * {@link #markup9} mutator.
	 * @param markup9	The new value.
	 **/
	@XmlElement
	public void setMarkup9(String markup9) {
		preset(markup9PropertyName, markup9);
		this.markup9 = markup9;
	}

	/**
	 * {@link #markup10} accessor.
	 * @return	The value.
	 **/
	public String getMarkup10() {
		return markup10;
	}

	/**
	 * {@link #markup10} mutator.
	 * @param markup10	The new value.
	 **/
	@XmlElement
	public void setMarkup10(String markup10) {
		preset(markup10PropertyName, markup10);
		this.markup10 = markup10;
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

	/**
	 * {@link #text5006} accessor.
	 * @return	The value.
	 **/
	public String getText5006() {
		return text5006;
	}

	/**
	 * {@link #text5006} mutator.
	 * @param text5006	The new value.
	 **/
	@XmlElement
	public void setText5006(String text5006) {
		preset(text5006PropertyName, text5006);
		this.text5006 = text5006;
	}

	/**
	 * {@link #text5007} accessor.
	 * @return	The value.
	 **/
	public String getText5007() {
		return text5007;
	}

	/**
	 * {@link #text5007} mutator.
	 * @param text5007	The new value.
	 **/
	@XmlElement
	public void setText5007(String text5007) {
		preset(text5007PropertyName, text5007);
		this.text5007 = text5007;
	}

	/**
	 * {@link #text5008} accessor.
	 * @return	The value.
	 **/
	public String getText5008() {
		return text5008;
	}

	/**
	 * {@link #text5008} mutator.
	 * @param text5008	The new value.
	 **/
	@XmlElement
	public void setText5008(String text5008) {
		preset(text5008PropertyName, text5008);
		this.text5008 = text5008;
	}

	/**
	 * {@link #text5009} accessor.
	 * @return	The value.
	 **/
	public String getText5009() {
		return text5009;
	}

	/**
	 * {@link #text5009} mutator.
	 * @param text5009	The new value.
	 **/
	@XmlElement
	public void setText5009(String text5009) {
		preset(text5009PropertyName, text5009);
		this.text5009 = text5009;
	}

	/**
	 * {@link #text50010} accessor.
	 * @return	The value.
	 **/
	public String getText50010() {
		return text50010;
	}

	/**
	 * {@link #text50010} mutator.
	 * @param text50010	The new value.
	 **/
	@XmlElement
	public void setText50010(String text50010) {
		preset(text50010PropertyName, text50010);
		this.text50010 = text50010;
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
