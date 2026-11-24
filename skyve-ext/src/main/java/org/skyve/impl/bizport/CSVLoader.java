package org.skyve.impl.bizport;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.model.document.ModuleDocument;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * {@link AbstractDataFileLoader} implementation that reads CSV files via
 * SuperCSV, mapping each header column to a Skyve binding expression.
 */
public class CSVLoader extends AbstractDataFileLoader {
	private InputStreamReader fileReader;
	private CsvMapReader csvReader;
	private String[] headers;
	private Map<String, String> valueMap;

	public @Nullable Map<String, String> getValueMap() {
		return valueMap;
	}

	public CSVLoader(@Nonnull LoaderActivityType activityType,
						@Nonnull InputStream fileInputStream,
						@Nonnull UploadException exception,
						@Nonnull String moduleName,
						@Nonnull String documentName,
						@Nonnull String... bindings) {

		super(activityType, exception, moduleName, documentName);
		addFields(bindings);

		fileReader = new InputStreamReader(fileInputStream, StandardCharsets.UTF_8);
		csvReader = new CsvMapReader(fileReader, CsvPreference.STANDARD_PREFERENCE);
		try {
			headers = csvReader.getHeader(true);
		}
		catch (IOException e) {
			throw new DomainException("Cannot read CSV header line", e);
		}

		dataIndex = 0;
	}

	/**
	 * Creates a CSV loader for the referenced document; a typed shortcut for
	 * {@link #CSVLoader(LoaderActivityType, InputStream, UploadException, String, String, String...)}.
	 *
	 * @param activityType	what to do with each row
	 * @param fileInputStream	the CSV content, with a header row
	 * @param exception	collects the problems found while loading
	 * @param document	the compile-time reference to the document to load into
	 * @param bindings	the bindings in the same order as the columns
	 */
	public CSVLoader(@Nonnull LoaderActivityType activityType,
						@Nonnull InputStream fileInputStream,
						@Nonnull UploadException exception,
						@Nonnull ModuleDocument document,
						@Nonnull String... bindings) {
		this(activityType, fileInputStream, exception, document.moduleName(), document.documentName(), bindings);
	}

	@Override
	public void finalize() {
		try {
			if (csvReader != null) {
				csvReader.close();
			}
			if (fileReader != null) {
				fileReader.close();
			}
		}
		catch (IOException e) {
			throw new DomainException("Could not close the CSV stream", e);
		}
	}

	@Override
	public @Nullable String getStringFieldValue(int index, boolean blankAsNull) {
		String hdr = headers[index];
		if (valueMap != null && valueMap.containsKey(hdr)) {
			String value = valueMap.get(hdr);
			if (blankAsNull && (value != null) && "".equals(value.trim())) {
				return null;
			}

			return value;
		}

		return null;
	}

	@Override
	public @Nullable Date getDateFieldValue(int index) {
		String value = getStringFieldValue(index, true);
		if (null == value)
			return null;

		Converter<DateTime> converter = CORE.getCustomer().getDefaultDateTimeConverter();
		Date result;
		try {
			result = converter.fromDisplayValue(value);
		}
		catch (Exception e) {
			throw new DomainException("Cannot convert date/time from " + value, e);
		}
		return result;
	}

	@Override
	public @Nonnull Double getNumericFieldValue(int index, boolean emptyAsZero) {
		String value = getStringFieldValue(index, true);
		if (null == value)
			return Double.valueOf(0);

		return Double.valueOf(value);
	}

	@Override
	public void nextData() {
		Map<String, String> values;
		try {
			values = csvReader.read(headers);
		} 
		catch (IOException e) {
			throw new DomainException("Cannot read CSV line " + dataIndex, e);
		}		
		valueMap = values;
		dataIndex++;
	}

	@Override
	public boolean hasNextData() {
		return true;
	}

	@Override
	public boolean isNoData() {
		return (valueMap == null);
	}

	@Override
	public @Nonnull String getWhere(int index) {
		StringBuilder where = new StringBuilder(128);
		where.append("Line ").append(dataIndex);
		where.append(" ").append(headers[index]);
		where.append(".");
		return where.toString();
	}

	@Override
	public @Nonnull String debugData() {
		StringBuilder sb = new StringBuilder();
		sb.append("Line ").append(dataIndex);

		if (valueMap != null) {
			for (String key : valueMap.keySet()) {
				sb.append(", (").append(dataIndex).append(",").append(key).append(") = ");
				sb.append(valueMap.get(key).toString());
			}
		}
		else {
			sb.append(" Null");
		}
		return sb.toString();
	}
}
