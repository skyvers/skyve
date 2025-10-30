package modules.admin.MonitoringDashboard.models;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.InMemoryListModel;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.Generic;
import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

public class RequestListModel extends InMemoryListModel<MonitoringDashboard> {
	private List<MetaDataQueryColumn> columns = new ArrayList<>();
	
	private static final Integer COLUMN_WIDTH = Integer.valueOf(125);

	@Override
	public List<Bean> getRows() throws Exception {
		List<Bean> result = new ArrayList<>();
		
		long now = System.currentTimeMillis();
		
		// Get all request key codes from monitoring
		for (String requestKeyCode : Monitoring.getRequestKeyCodes()) {
			Map<Timestamp, Generic> results = new TreeMap<>();
			RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKeyCode);
			if (measurements != null) {
				RequestKey requestKey = RequestKey.fromString(requestKeyCode);
				
				// Current Minute
				processPeriod(requestKey,
								now, 
								measurements.getSecondsMillis(),
								measurements.getSecondsCpuUtilisation(),
								measurements.getSecondsSystemCpuUsage(),
								measurements.getSecondsHeapRamUsage(),
								Period.currentMinute,
								results);

				// Current Hour
				processPeriod(requestKey,
								now, 
								measurements.getMinutesMillis(),
								measurements.getMinutesCpuUtilisation(),
								measurements.getMinutesSystemCpuUsage(),
								measurements.getMinutesHeapRamUsage(),
								Period.currentHour,
								results);

				// Current Day
				processPeriod(requestKey,
								now, 
								measurements.getHoursMillis(),
								measurements.getHoursCpuUtilisation(),
								measurements.getHoursSystemCpuUsage(),
								measurements.getHoursHeapRamUsage(),
								Period.currentDay,
								results);

				// Current Week
				processPeriod(requestKey,
								now, 
								measurements.getDaysMillis(),
								measurements.getDaysCpuUtilisation(),
								measurements.getDaysSystemCpuUsage(),
								measurements.getDaysHeapRamUsage(),
								Period.currentWeek,
								results);

				// Current Year
				processPeriod(requestKey,
								now, 
								measurements.getWeeksMillis(),
								measurements.getWeeksCpuUtilisation(),
								measurements.getWeeksSystemCpuUsage(),
								measurements.getWeeksHeapRamUsage(),
								Period.currentYear,
								results);

				result.addAll(results.values());
			}
		}
		
		return result;
	}

	private static void processPeriod(RequestKey requestKey,
										long now,
										Map<Integer, Integer> millis,
										Map<Integer, Float> cpuUtilisation,
										Map<Integer, Float> systemCpuUsage,
										Map<Integer, Float> heapRamUsage,
										Period period,
										Map<Timestamp, Generic> results) {
		for (Entry<Integer, Integer> time :millis.entrySet()) {
			Generic row = rowMeUp(results, time, requestKey, now, period);
			row.setInteger1(time.getValue());
		}

		for (Entry<Integer, Float> cpu : cpuUtilisation.entrySet()) {
			Generic row = rowMeUp(results, cpu, requestKey, now, period);
			row.setDecimal21(new Decimal2(cpu.getValue().doubleValue()));
		}

		for (Entry<Integer, Float> cpu : systemCpuUsage.entrySet()) {
			Generic row = rowMeUp(results, cpu, requestKey, now, period);
			row.setDecimal22(new Decimal2(cpu.getValue().doubleValue()));
		}

		for (Entry<Integer, Float> ram : heapRamUsage.entrySet()) {
			Generic row = rowMeUp(results, ram, requestKey, now, period);
			row.setDecimal23(new Decimal2(ram.getValue().doubleValue()));
		}
	}

	private static Generic rowMeUp(Map<Timestamp, Generic> rows,
									Entry<Integer, ? extends Number> entry,
									RequestKey requestKey,
									long now,
									Period period) {
		Timestamp time = new Timestamp(calculateTimestampForIndex(now, entry.getKey().intValue(), period));
		return rows.computeIfAbsent(time, k -> {
			Generic g = Generic.newInstance();
			g.setTimestamp1(time);
			g.setMemo1(requestKey.toDescription());
			return g;
		});
	}
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// Set driving document
		Module module = customer.getModule(Generic.MODULE_NAME);
		Document document = module.getDocument(customer, Generic.DOCUMENT_NAME);
		setDrivingDocument(module, document);

		// Add columns
		defineColumn(Generic.timestamp1PropertyName, "Time", null);
		defineColumn(Generic.memo1PropertyName, "Request", null);
		defineColumn(Generic.integer1PropertyName, "Req MS", Integer.valueOf(75));
		defineColumn(Generic.decimal21PropertyName, "Req CPU (%)", COLUMN_WIDTH);
		defineColumn(Generic.decimal22PropertyName, "Sys CPU (%)", COLUMN_WIDTH);
		defineColumn(Generic.decimal23PropertyName, "Heap (%)", COLUMN_WIDTH);

		super.postConstruct(customer, runtime);
	}

	private void defineColumn(final String binding, final String displayName, Integer pixelWidth) {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setDisplayName(displayName);
		column.setBinding(binding);
		column.setEditable(false);
		column.setFilterable(false);
		column.setPixelWidth(pixelWidth);

		columns.add(column);
	}

	@Override
	public String getDescription() {
		return "Requests";
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("Not implemented");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("Not implemented");
	}
	
	/**
	 * Calculate timestamp by replacing the appropriate time component with the given index.
	 * This ensures timestamps align with the actual time structure rather than using subtraction.
	 */
	static long calculateTimestampForIndex(long currentTime, int index, Period period) {
		LocalDateTime now = LocalDateTime.ofInstant(Instant.ofEpochMilli(currentTime), ZoneId.systemDefault());
		LocalDateTime timestamp;

		switch (period) {
			case currentMinute:
				// Replace seconds with index (0-59)
				timestamp = now.withSecond(index);
				break;
			case currentHour:
				// Replace minutes with index (0-59)
				timestamp = now.withSecond(0).withMinute(index);
				break;
			case currentDay:
				// Replace hours with index (0-23)
				timestamp = now.withSecond(0).withMinute(0).withHour(index);
				break;
			case currentWeek:
				// Replace day of week with index (0-6, so add 1 1=Monday, 7=Sunday)
				timestamp = now.withSecond(0).withMinute(0).withHour(0).with(ChronoField.DAY_OF_WEEK, index + 1L);
				break;
			case currentYear:
				// Replace week of year with index (0-51, so add 1 for 1-52)
				timestamp = now.withSecond(0).withMinute(0).withHour(0).withDayOfYear(1).with(ChronoField.ALIGNED_WEEK_OF_YEAR, index + 1L);
				break;
			default:
				timestamp = now;
				break;
		}

		return timestamp.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
	}
	
	/**
	 * Format a timestamp for display based on the time period.
	 */
	static String formatTimestampLabel(long timestampMillis, Period period) {
		LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());

		return switch (period) {
			case currentMinute -> dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));
			case currentHour -> dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
			case currentDay -> dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));
			case currentWeek -> dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			case currentYear -> dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
		};
	}

	static Map<Integer, Integer> extractMillisForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsMillis();
			case currentHour:
				return measurements.getMinutesMillis();
			case currentDay:
				return measurements.getHoursMillis();
			case currentWeek:
				return measurements.getDaysMillis();
			case currentYear:
				return measurements.getWeeksMillis();
			default:
				return measurements.getHoursMillis();
		}
	}

	static Map<Integer, Float> extractCpuUtilisationForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsCpuUtilisation();
			case currentHour:
				return measurements.getMinutesCpuUtilisation();
			case currentDay:
				return measurements.getHoursCpuUtilisation();
			case currentWeek:
				return measurements.getDaysCpuUtilisation();
			case currentYear:
				return measurements.getWeeksCpuUtilisation();
			default:
				return measurements.getHoursCpuUtilisation();
		}
	}

	static Map<Integer, Float> extractSystemCpuUsageForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsSystemCpuUsage();
			case currentHour:
				return measurements.getMinutesSystemCpuUsage();
			case currentDay:
				return measurements.getHoursSystemCpuUsage();
			case currentWeek:
				return measurements.getDaysSystemCpuUsage();
			case currentYear:
				return measurements.getWeeksSystemCpuUsage();
			default:
				return measurements.getHoursSystemCpuUsage();
		}
	}

	static Map<Integer, Float> extractHeapRamUsageForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsHeapRamUsage();
			case currentHour:
				return measurements.getMinutesHeapRamUsage();
			case currentDay:
				return measurements.getHoursHeapRamUsage();
			case currentWeek:
				return measurements.getDaysHeapRamUsage();
			case currentYear:
				return measurements.getWeeksHeapRamUsage();
			default:
				return measurements.getHoursHeapRamUsage();
		}
	}
}
