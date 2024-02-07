package modules.admin.ReportDataset;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.report.freemarker.BeanReportDataset;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.util.Time;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.spi.CDI;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportParameter;

public class ReportDatasetExtension extends ReportDataset {

	private static final long serialVersionUID = -688307133122437337L;
	private static final Logger LOGGER = LoggerFactory.getLogger(ReportDatasetExtension.class);

	/**
	 * Regular expression to locate date sentinel values within a query
	 */
	private static final String BIZQL_DATE_PATTERN = "\\{DATE\\s?(([+-]\\d{1,})([dDmMyY]))?\\}";
	/**
	 * Regular expression to locate named parameters within a query
	 */
	private static final String NAMED_PARAMETER_PATTERN = ":([\\p{L}|_]+)";

	private static final String PARAMETER_PREFIX = ":";
	private static final String DATE_PARAMETER_STRING_FORMAT = "d_%s_%d";
	private static final DateTimeFormatter DATE_PARAMETER_DATE_FORMAT = DateTimeFormatter.ofPattern("yyyyMMddHHmm");

	/**
	 * If this is a SQL or BizQL dataset, parse the query and check for any named parameters.
	 * If any parameters haven't already been created, create a new optional parameter in the template
	 * with the name.
	 */
	public void addMissingParameters() {
		if (isTypeQuery()) {
			// check we have a query and a parent
			if (getQuery() == null || getParent() == null) {
				return;
			}

			// parse and check for parameters
			Set<String> namedParameters = new HashSet<>();

			Pattern pattern = Pattern.compile(NAMED_PARAMETER_PATTERN);
			Matcher matcher = pattern.matcher(getQuery());

			while (matcher.find()) {
				final String match = matcher.group(1);
				namedParameters.add(match);
			}

			List<ReportParameterExtension> existingParameters = getParent().getParameters();

			if (existingParameters.size() == 0) {
				// add all found parameters
				for (String param : namedParameters) {
					existingParameters.add(createNewParameter(param));
				}
			} else {
				// check for any new parameters
				for (String param : namedParameters) {
					if (existingParameters.stream().noneMatch(p -> p.getName().contentEquals(param))) {
						existingParameters.add(createNewParameter(param));
					}
				}
			}
		}
	}

	/**
	 * Checks if the query in this dataset contains the specified parameter. Used
	 * when testing and executing the query in case the parameter is defined for the
	 * template but is not required for this dataset.
	 * 
	 * This only applies for BizQL and SQL datasets.
	 * 
	 * @param parameter The parameter to check if is in use in this dataset
	 * @return true if the parameter is in use, false otherwise
	 */
	public boolean containsParameter(final ReportParameterExtension parameter) {
		if(getDatasetType() == DatasetType.bizQL || getDatasetType() == DatasetType.SQL) {
			if (parameter == null) {
				throw new DomainException("Parameter is required.");
			}

			if (getQuery().contains(PARAMETER_PREFIX + parameter.getName())) {
				return true;
			}

			return false;
		}
		
		return true;
	}

	/**
	 * Executes the BeanReportDataset class specified in this ReportDataset and injects
	 * any supplied parameters.
	 * 
	 * @return The list of beans from the class dataset
	 */
	@Override
	public List<DynaBean> executeClass() {
		try {
			@SuppressWarnings("unchecked")
			Class<BeanReportDataset> reportClass = (Class<BeanReportDataset>) Thread.currentThread().getContextClassLoader().loadClass(getQuery());
			if (reportClass != null) {
				BeanReportDataset dataset = CDI.current().select(reportClass).get();
				return dataset.getResults(getParent().getParameters());

			}
		} catch (Exception e) {
			throw new DomainException("Unable to create an instance of " + getQuery(), e);
		}
		return null;
	}

	/**
	 * Executes the BizQL query supplied in a ReportDataset and injects any supplied parameters in
	 * use by the dataset query.
	 * 
	 * @return The list of beans from the query
	 * @throws Exception
	 */
	@Override
	public List<Bean> executeQuery() throws Exception {
		if (DatasetType.bizQL != getDatasetType()) {
			throw new IllegalArgumentException(String.format("Dataset type must be %s", DatasetType.bizQL.toLocalisedDescription()));
		}

		SubstitutedQueryResult sQR = getSubstitutedQuery();
		BizQL bql = CORE.getPersistence().newBizQL(sQR.getQuery());
		
		// put any parameters
		for (ReportParameterExtension param : getParent().getParameters()) {
			if (containsParameter(param)) {
				switch (param.getType()) {
					case date:
						if (param.getReportInputValue() != null) {
							DateOnly date = CORE.getCustomer().getDefaultDateConverter()
									.fromDisplayValue(param.getReportInputValue());
							bql.putParameter(param.getName(), date);
						} else {
							bql.putParameter(param.getName(), param.getDateDefaultValue());
						}
						break;
					case integer:
						if (param.getReportInputValue() != null) {
							bql.putParameter(param.getName(), Integer.valueOf(param.getReportInputValue()));
						} else {
							bql.putParameter(param.getName(), (param.getNumericalDefaultValue() == null ? null
									: Integer.valueOf(param.getNumericalDefaultValue().intValue())));
						}
						break;
					case longInteger:
						if (param.getReportInputValue() != null) {
							bql.putParameter(param.getName(), Long.valueOf(param.getReportInputValue()));
						} else {
							bql.putParameter(param.getName(), param.getNumericalDefaultValue());
						}
						break;
					default:
						if (param.getReportInputValue() != null) {
							bql.putParameter(param.getName(), param.getReportInputValue());
						} else {
							bql.putParameter(param.getName(), param.getTextDefaultValue());
						}
				}
			}
		}

		// put any substituted date parameters
		for (Map.Entry<String, DateOnly> entry : sQR.getParameters().entrySet()) {
			bql.putParameter(entry.getKey(), entry.getValue());
		}

		return bql.beanResults();
	}

	/**
	 * Executes the SQL query supplied in a ReportDataset using the supplied parameter values
	 * in use by the dataset.
	 * 
	 * @return The list of beans from the query
	 */
	@Override
	public List<DynaBean> executeSQLQuery() throws Exception {
		if (DatasetType.SQL != getDatasetType()) {
			throw new IllegalArgumentException(String.format("Dataset type must be %s", DatasetType.SQL.toLocalisedDescription()));
		}

		final SQL sql = CORE.getPersistence().newSQL(getQuery());
		
		// put any parameters
		for (ReportParameterExtension param : getParent().getParameters()) {
			if (containsParameter(param)) {
				switch (param.getType()) {
					case date:
						if (param.getReportInputValue() != null) {
							DateOnly date = CORE.getCustomer().getDefaultDateConverter()
									.fromDisplayValue(param.getReportInputValue());
							sql.putParameter(param.getName(), date);
						} else {
							sql.putParameter(param.getName(), param.getDateDefaultValue());
						}
						break;
					case integer:
						if (param.getReportInputValue() != null) {
							sql.putParameter(param.getName(), Integer.valueOf(param.getReportInputValue()));
						} else {
							sql.putParameter(param.getName(), (param.getNumericalDefaultValue() == null ? null
									: Integer.valueOf(param.getNumericalDefaultValue().intValue())));
						}
						break;
					case longInteger:
						if (param.getReportInputValue() != null) {
							sql.putParameter(param.getName(), Long.getLong(param.getReportInputValue()));
						} else {
							sql.putParameter(param.getName(), param.getNumericalDefaultValue());
						}
						break;
					default:
						if (param.getReportInputValue() != null) {
							sql.putParameter(param.getName(), param.getReportInputValue(), false);
						} else {
							sql.putParameter(param.getName(), param.getTextDefaultValue(), false);
						}
				}
			}
		}
		
		return sql.dynaResults();
	}

	/**
	 * Executes the BeanReportDataset class specified in this ReportDataset and injects
	 * any supplied test parameters.
	 * 
	 * @return The list of beans from the class dataset
	 */
	public List<DynaBean> executeTestClass() {
		return executeClass();
	}

	/**
	 * Executes the BizQL query supplied in a ReportDataset using the supplied test parameter
	 * values in use by the dataset query.
	 * 
	 * @return The list of beans from the query
	 */
	public List<Bean> executeTestQuery() {
		if (DatasetType.bizQL != getDatasetType()) {
			throw new IllegalArgumentException(String.format("Dataset type must be %s", DatasetType.bizQL.toLocalisedDescription()));
		}

		SubstitutedQueryResult sQR = getSubstitutedQuery();
		BizQL bql = CORE.getPersistence().newBizQL(sQR.getQuery());

		// put any parameters
		for (ReportParameterExtension param : getParent().getParameters()) {
			if (containsParameter(param)) {
				switch (param.getType()) {
					case date:
						bql.putParameter(param.getName(), param.getDateTestValue());
						break;
					case integer:
						bql.putParameter(param.getName(),
								(param.getNumericalTestValue() == null ? null : Integer.valueOf(param.getNumericalTestValue().intValue())));
						break;
					case longInteger:
						bql.putParameter(param.getName(), param.getNumericalTestValue());
						break;
					default:
						bql.putParameter(param.getName(), param.getTextTestValue());
				}
			}
		}

		// put any substituted date parameters
		for (Map.Entry<String, DateOnly> entry : sQR.getParameters().entrySet()) {
			bql.putParameter(entry.getKey(), entry.getValue());
		}

		return bql.beanResults();
	}

	/**
	 * Executes the SQL query supplied in a ReportDataset using the supplied test parameter values
	 * in use by the dataset query.
	 * 
	 * @return The list of beans from the query
	 */
	public List<DynaBean> executeTestSQLQuery() throws Exception {
		if (DatasetType.SQL != getDatasetType()) {
			throw new IllegalArgumentException(String.format("Dataset type must be %s", DatasetType.SQL.toLocalisedDescription()));
		}

		final SQL sql = CORE.getPersistence().newSQL(getQuery());
		
		for (ReportParameterExtension param : getParent().getParameters()) {
			if (containsParameter(param)) {
				switch (param.getType()) {
					case date:
						sql.putParameter(param.getName(), param.getDateTestValue());
						break;
					case integer:
						sql.putParameter(param.getName(),
								(param.getNumericalTestValue() == null ? null : Integer.valueOf(param.getNumericalTestValue().intValue())));
						break;
					case longInteger:
						sql.putParameter(param.getName(), param.getNumericalTestValue());
						break;
					default:
						sql.putParameter(param.getName(), param.getTextTestValue(), false);
				}
			}
		}
		
		return sql.dynaResults();
	}

	/**
	 * If this dataset is BizQL, returns a substituted query replacing any date/datetime
	 * sentinels with a Java date value. If this is not a BizQL dataset, returns the query.
	 * 
	 * @return A substituted query replacing any date sentinels with the date parameters to use, or the original query
	 */
	public SubstitutedQueryResult getSubstitutedQuery() {
		if (getDatasetType() == DatasetType.bizQL) {
			String query = getQuery();
			if (query.contains("{DATE")) {
				Map<String, DateOnly> dateParameters = new HashMap<>();

				Pattern pattern = Pattern.compile(BIZQL_DATE_PATTERN);
				Matcher matcher = pattern.matcher(getQuery());

				int dateCount = 1;

				while (matcher.find()) {
					final String dateExpression = matcher.group(0);
					LOGGER.debug("Matched date expression: {}", dateExpression);

					DateOnly replacementDate = new DateOnly();

					if (matcher.group(1) != null) {
						// System.out.println(matcher.group(1));
						// System.out.println(matcher.group(2));
						// System.out.println(matcher.group(3));

						final int countModifier = Integer.parseInt(matcher.group(2));
						final String periodModifier = matcher.group(3);

						switch (periodModifier) {
							case "d":
								// add/subtract days
								Time.addDays(replacementDate, countModifier);
								break;
							case "m":
								// add/subtract months
								Time.addMonths(replacementDate, countModifier);
								break;
							case "y":
								// add/subtract years
								replacementDate = Time.addYearsToNew(replacementDate, countModifier);
								break;
							default:
								throw new IllegalStateException(periodModifier + " is not catered for");
						}
					}

					// check it hasn't already been replaced
					if (query.contains(dateExpression)) {
						String dateParameterName = String.format(DATE_PARAMETER_STRING_FORMAT,
								DATE_PARAMETER_DATE_FORMAT.format(LocalDateTime.now()), Integer.valueOf(dateCount));

						// update the original query string to use the new date parameter name
						query = query.replace(dateExpression, PARAMETER_PREFIX + dateParameterName);
						LOGGER.info(String.format("Replaced %s with %s.", dateExpression, replacementDate));

						// add the date to the parameter list
						dateParameters.put(dateParameterName, replacementDate);
					}

					dateCount++;
				}

				if (query.contains("{DATE")) {
					// throw exception if we made it through the regular expression without replacing anything
					throw new IllegalArgumentException(
							"Invalid date expression, expected {DATE} or a valid date modifier format, e.g. {DATE+1d}");
				}

				return new SubstitutedQueryResult(query, dateParameters);
			}
		}

		return new SubstitutedQueryResult(getQuery());
	}

	/**
	 * Creates a new parameter against the parent ReportTemplate of this dataset.
	 * 
	 * @param parameterName The name of the new parameter
	 * @return The new parameter with the name and description defaulted
	 */
	static ReportParameterExtension createNewParameter(String parameterName) {
		ReportParameterExtension newParam = ReportParameter.newInstance();
		newParam.setName(parameterName);
		newParam.setDescription(Binder.toTitleCase(parameterName));

		// if the parameter name ends with "date", presume the type to be date
		if (parameterName.endsWith("Date")) {
			newParam.setType(Type.date);
		}

		return newParam;
	}

	/**
	 * Class to hold the return type of a substituted BizQL query.
	 * The original query is modified to replace any date expressions (<code>{DATE}</code>),
	 * with named parameters and the parameters and their date values are stored in the map.
	 */
	public class SubstitutedQueryResult {
		private final String query;
		private final Map<String, DateOnly> parameters;

		public SubstitutedQueryResult(String query) {
			this.query = query;
			this.parameters = new HashMap<>();
		}

		public SubstitutedQueryResult(final String query, final Map<String, DateOnly> parameters) {
			this.query = query;
			this.parameters = parameters != null ? parameters : new HashMap<>();
		}

		public String getQuery() {
			return query;
		}

		public Map<String, DateOnly> getParameters() {
			return parameters;
		}
	}
}
