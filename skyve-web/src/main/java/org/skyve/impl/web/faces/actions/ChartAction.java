package org.skyve.impl.web.faces.actions;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.primefaces.model.charts.ChartData;
import org.primefaces.model.charts.ChartModel;
import org.primefaces.model.charts.bar.BarChartDataSet;
import org.primefaces.model.charts.bar.BarChartModel;
import org.primefaces.model.charts.bar.BarChartOptions;
import org.primefaces.model.charts.donut.DonutChartDataSet;
import org.primefaces.model.charts.donut.DonutChartModel;
import org.primefaces.model.charts.donut.DonutChartOptions;
import org.primefaces.model.charts.hbar.HorizontalBarChartDataSet;
import org.primefaces.model.charts.hbar.HorizontalBarChartModel;
import org.primefaces.model.charts.line.LineChartDataSet;
import org.primefaces.model.charts.line.LineChartModel;
import org.primefaces.model.charts.line.LineChartOptions;
import org.primefaces.model.charts.optionconfig.title.Title;
import org.primefaces.model.charts.pie.PieChartDataSet;
import org.primefaces.model.charts.pie.PieChartModel;
import org.primefaces.model.charts.pie.PieChartOptions;
import org.primefaces.model.charts.polar.PolarAreaChartDataSet;
import org.primefaces.model.charts.polar.PolarAreaChartModel;
import org.primefaces.model.charts.polar.PolarAreaChartOptions;
import org.primefaces.model.charts.radar.RadarChartDataSet;
import org.primefaces.model.charts.radar.RadarChartModel;
import org.primefaces.model.charts.radar.RadarChartOptions;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.charts.PrimeFacesChartPostProcessor;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.chart.MetaDataChartModel;

/**
 * Create a PF chart model from a Skyve model.
 */
public class ChartAction extends FacesAction<ChartModel> {
	private FacesView facesView;
	private Object model;
	private ChartType type;
	
	public ChartAction(FacesView facesView, Object model, ChartType type) {
		this.facesView = facesView;
		this.model = model;
		this.type = type;
	}

	@Override
	public ChartModel callback() throws Exception {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.info("ChartAction - CHART " + model);

		AbstractPersistence persistence = AbstractPersistence.get();
		Bean targetBean = ActionUtil.getTargetBeanForView(facesView);
    	User user = persistence.getUser();
    	Customer customer = user.getCustomer();
    	Module targetModule = customer.getModule(targetBean.getBizModule());
		Document targetDocument = targetModule.getDocument(customer, targetBean.getBizDocument());
		org.skyve.metadata.view.model.chart.ChartModel<Bean> chartModel = null;
		if (model instanceof String) {
			chartModel = targetDocument.getChartModel(customer, (String) model, true);
		}
		else {
			chartModel = new MetaDataChartModel((ChartBuilderMetaData) model);
		}
		chartModel.setBean(targetBean);
		org.skyve.metadata.view.model.chart.ChartData data = chartModel.getChartData();

		ChartModel result = pfChartModel(type, data);
		result.setExtender("SKYVE.PF.chartExtender");
		return result;
	}
	
	public static ChartModel pfChartModel(ChartType type, org.skyve.metadata.view.model.chart.ChartData data) {
		Title title = title(data);

		ChartModel result = null;
		
		boolean horizontal = ChartType.horizontalBar.equals(type);
		if (ChartType.bar.equals(type) || horizontal) {
			BarChartDataSet set = horizontal ? new HorizontalBarChartDataSet() : new BarChartDataSet();
			set.setData(data.getValues());
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setBorderWidth(Integer.valueOf(1));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			BarChartModel barChartModel = horizontal ? new HorizontalBarChartModel() : new BarChartModel();
			barChartModel.setData(chartData);
			
			if (title != null) {
				BarChartOptions options = new BarChartOptions();
				options.setTitle(title);
				barChartModel.setOptions(options);
			}

			result = barChartModel;
		}
		else if (ChartType.doughnut.equals(type)) {
			DonutChartDataSet set = new DonutChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			DonutChartModel donutChartModel = new DonutChartModel();
			donutChartModel.setData(chartData);
			
			if (title != null) {
				DonutChartOptions options = new DonutChartOptions();
				options.setTitle(title);
				donutChartModel.setOptions(options);
			}

			result = donutChartModel;
		}
		else if (ChartType.line.equals(type) || ChartType.lineArea.equals(type)) {
			LineChartDataSet set = new LineChartDataSet();
			@SuppressWarnings("unchecked")
			List<Object> objects = (List<Object>) (List<?>) data.getValues();
			set.setData(objects);
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setPointBackgroundColor(set.getBorderColor());
			set.setPointBorderColor(set.getBorderColor());
			set.setBorderWidth(Integer.valueOf(1));
			if (ChartType.line.equals(type)) {
				set.setFill(Boolean.FALSE);
			}
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			LineChartModel lineChartModel = new LineChartModel();
			lineChartModel.setData(chartData);
			
			if (title != null) {
				LineChartOptions options = new LineChartOptions();
				options.setTitle(title);
				lineChartModel.setOptions(options);
			}

			result = lineChartModel;
		}
		else if (ChartType.pie.equals(type)) {
			PieChartDataSet set = new PieChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			PieChartModel pieChartModel = new PieChartModel();
			pieChartModel.setData(chartData);
			
			if (title != null) {
				PieChartOptions options = new PieChartOptions();
				options.setTitle(title);
				pieChartModel.setOptions(options);
			}

			result = pieChartModel;
		}
		else if (ChartType.polarArea.equals(type)) {
			PolarAreaChartDataSet set = new PolarAreaChartDataSet();
			set.setData(data.getValues());
			set.setBackgroundColor(web(data.getBackgrounds()));
			set.setBorderColor(web(data.getBorders()));
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			PolarAreaChartModel polarChartModel = new PolarAreaChartModel();
			polarChartModel.setData(chartData);
			
			if (title != null) {
				PolarAreaChartOptions options = new PolarAreaChartOptions();
				options.setTitle(title);
				polarChartModel.setOptions(options);
			}
			
			result = polarChartModel;
		}
		else if (ChartType.radar.equals(type)) {
			RadarChartDataSet set = new RadarChartDataSet();
			set.setData(data.getValues());
			set.setLabel(data.getLabel());
			set.setBackgroundColor(web(data.getBackground()));
			set.setBorderColor(web(data.getBorder()));
			set.setBorderWidth(Integer.valueOf(1));
			set.setPointBackgroundColor(set.getBorderColor());
			set.setPointBorderColor(set.getBorderColor());
			ChartData chartData = new ChartData();
			chartData.addChartDataSet(set);
			chartData.setLabels(data.getLabels());
			RadarChartModel radarChartModel = new RadarChartModel();
			radarChartModel.setData(chartData);

			if (title != null) {
				RadarChartOptions options = new RadarChartOptions();
				options.setTitle(title);
				radarChartModel.setOptions(options);
			}

			result = radarChartModel;
		}
		else {
			throw new IllegalArgumentException("Chart Type " + type + " is not supported.");
		}
		
		postProcess(result, data);
		
		return result;
	}
	
	private static Title title(org.skyve.metadata.view.model.chart.ChartData data) {
		Title result = null;
		String text = data.getTitle();
		if (text != null) {
			result = new Title();
			result.setDisplay(true);
			result.setText(text);
		}
		return result;
	}
	
	private static String web(Color colour) {
		if (colour == null) {
			return null;
		}
		int alpha = colour.getAlpha();
		if (alpha > 0) {
			return String.format("rgba(%d,%d,%d,%.2f)",
									Integer.valueOf(colour.getRed()),
									Integer.valueOf(colour.getGreen()),
									Integer.valueOf(colour.getBlue()),
									Float.valueOf(alpha / 255F));
		}
		return new StringBuilder(16).append("rgb(").append(colour.getRed()).append(',').append(colour.getGreen()).append(',').append(colour.getBlue()).append(')').toString();
	}
	
	private static List<String> web(List<Color> colours) {
		if (colours == null) {
			return null;
		}
		List<String> result = new ArrayList<>(colours.size());
		for (Color colour : colours) {
			result.add(web(colour));
		}
		return result;
	}
	
	private static void postProcess(ChartModel model,
										org.skyve.metadata.view.model.chart.ChartData data) {
		String postProcessor = data.getPrimeFacesChartPostProcessorClassName();
		if (postProcessor == null) {
			Customer c = CORE.getCustomer();
			postProcessor = c.getPrimeFacesChartPostProcessorClassName();
		}
		if (postProcessor != null) {
			try {
				Class<?> instanceClass = Thread.currentThread().getContextClassLoader().loadClass(postProcessor);
				@SuppressWarnings("unchecked")
				PrimeFacesChartPostProcessor<ChartModel> instance = (PrimeFacesChartPostProcessor<ChartModel>) instanceClass.getDeclaredConstructor().newInstance();
				instance.process(model);
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not create chart post processor", e);
			}
		}
	}
}
