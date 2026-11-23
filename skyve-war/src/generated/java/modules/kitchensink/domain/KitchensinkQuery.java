package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the metadata queries declared in the kitchensink module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum KitchensinkQuery implements ModuleQuery {
	/** The "qEscapingFixture" metadata query. */
	Q_ESCAPING_FIXTURE("qEscapingFixture"),
	/** The "qModelLookupValues" metadata query. */
	Q_MODEL_LOOKUP_VALUES("qModelLookupValues"),
	/** The "qModelLookupFixtures" metadata query. */
	Q_MODEL_LOOKUP_FIXTURES("qModelLookupFixtures"),
	/** The "qListGridFixtureVue" metadata query. */
	Q_LIST_GRID_FIXTURE_VUE("qListGridFixtureVue"),
	/** The "qListGridFixtureFaces" metadata query. */
	Q_LIST_GRID_FIXTURE_FACES("qListGridFixtureFaces"),
	/** The "qListGridFixtureVueAttributes" metadata query. */
	Q_LIST_GRID_FIXTURE_VUE_ATTRIBUTES("qListGridFixtureVueAttributes"),
	/** The "qListGridFixtureFacesAttributes" metadata query. */
	Q_LIST_GRID_FIXTURE_FACES_ATTRIBUTES("qListGridFixtureFacesAttributes"),
	/** The "qConditionsFixtureListAttributes" metadata query. */
	Q_CONDITIONS_FIXTURE_LIST_ATTRIBUTES("qConditionsFixtureListAttributes"),
	/** The "qConditionsFixtureNodes" metadata query. */
	Q_CONDITIONS_FIXTURE_NODES("qConditionsFixtureNodes"),
	/** The "qConvertersFixtures" metadata query. */
	Q_CONVERTERS_FIXTURES("qConvertersFixtures"),
	/** The "qConvertersDates" metadata query. */
	Q_CONVERTERS_DATES("qConvertersDates"),
	/** The "qConvertersDateTimes" metadata query. */
	Q_CONVERTERS_DATE_TIMES("qConvertersDateTimes"),
	/** The "qConvertersTimestamps" metadata query. */
	Q_CONVERTERS_TIMESTAMPS("qConvertersTimestamps"),
	/** The "qConvertersDateOnly" metadata query. */
	Q_CONVERTERS_DATE_ONLY("qConvertersDateOnly"),
	/** The "qConvertersTimes" metadata query. */
	Q_CONVERTERS_TIMES("qConvertersTimes"),
	/** The "qConvertersNumbers" metadata query. */
	Q_CONVERTERS_NUMBERS("qConvertersNumbers");

	private final String name;

	private KitchensinkQuery(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "kitchensink";
	}

	@Override
	public String queryName() {
		return name;
	}
}
