package org.skyve.impl.web.service;

import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.imageio.ImageIO;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;
import org.skyve.util.JSON;
import org.skyve.util.Util;

/**
 * Service for previous values.
 */
public class TrackmateServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public static final String CREATED_TIMESTAMP_PROPERTY_NAME = "createdTimestamp";
	public static final String DESCRIPTION_PROPERTY_NAME = "description";
	public static final String TRACK_DISCRIMINATOR_PROPERTY_NAME = "discriminator";
	public static final String TRACK_MODE_PROPERTY_NAME = "mode";
	public static final String TRACK_TIMESTAMP_PROPERTY_NAME = "timestamp";
	public static final String TRACK_INTEGRITY_HASH_PROPERTY_NAME = "originalDocumentHash";
	public static final String TRACK_USER_PROPERTY_NAME = "user";	
	public static final String DISCRIMINATOR_DESCRIPTION_PROPERTY_NAME = "description";
	
	public static final String PHOTO_PROPERTY_NAME = "photo";
	public static final String GEOMETRY_PROPERTY_NAME = "geometry";
	public static final String VERTICES_PROPERTY_NAME = "vertices";
	public static final String ADDRESS_PROPERTY_NAME = "address";
	public static final String CITY_PROPERTY_NAME = "city";
	public static final String POST_CODE_PROPERTY_NAME = "postCode";
	public static final String STATE_PROPERTY_NAME = "state";
	public static final String COUNTRY_PROPERTY_NAME = "country";

	public static final String TRACK_MODULE_NAME = "track";
	public static final String TRACK_DOCUMENT_NAME = "Track";
	public static final String DISCRIMINATOR_DOCUMENT_NAME = "Discriminator";
	public static final String ADMIN_USER_DOCUMENT_NAME = "User";
	
	public static final String GET_TYPE_DOMAIN_LIST = "domainList";
	public static final String GET_TYPE_USER_POSITION_LIST = "userPositionList";
	
	public static final String CUSTOMER_REQUEST_PARAMETER = "customer";
	public static final String USERNAME_REQUEST_PARAMETER = "username";
	public static final String PASSWORD_REQUEST_PARAMETER = "password";
	public static final String SESSION_REQUEST_PARAMETER = "sid";
	public static final String POST_ACTION_PARAMETER = "action";
	public static final String LOGIN_ACTION_NAME = "login";
	public static final String TRACK_ACTION_NAME = "track";
	public static final String MODULE_REQUEST_PARAMETER = "moduleName";
	public static final String DOCUMENT_REQUEST_PARAMETER = "documentName";
	public static final String GET_TYPE_REQUEST_PARAMETER = "getType";
	
	public static final String GEOLOCATION_DOCUMENT_NAME = "GeoLocation";
	public static final String TRACK_TYPE_PROPERTY_NAME = "type";
	public static final String TRACK_TYPE_POINT_VALUE = "P";
	public static final String TRACK_TYPE_POLYLINE_VALUE = "L";
	public static final String TRACK_TYPE_POLYGON_VALUE = "G";
	
	public static final String TRACK_MODE_ALERT_VALUE = "Alert";
	public static final String TRACK_MODE_EVIDENCE_VALUE = "Evidence";
	public static final String TRACK_MODE_STATUS_VALUE = "Status";
	
	public static final String VERTEX_TIMESTAMP_PROPERTY_NAME = "timestamp";
	public static final String VERTEX_LATITUDE_PROPERTY_NAME = "latitudeInDecimalDegrees";
	public static final String VERTEX_LONGITUDE_PROPERTY_NAME = "longitudeInDecimalDegrees";
	public static final String VERTEX_POSITION_ACCURACY_PROPERTY_NAME = "positionAccuracyInMetres";
	public static final String VERTEX_ALTITUDE_PROPERTY_NAME = "altitudeInMetres";
	public static final String VERTEX_ALTITUDE_ACCURACY_PROPERTY_NAME = "altitudeAccuracyInMetres";
	public static final String VERTEX_SPEED_PROPERTY_NAME = "speedInMetresPerSecond";
	public static final String VERTEX_MAGNETIC_HEADING_PROPERTY_NAME = "magneticHeadingInDecimalDegrees";
	public static final String VERTEX_TRUE_HEADING_PROPERTY_NAME = "trueHeadingInDecimalDegrees";
	public static final String VERTEX_HEADING_ACCURACY_PROPERTY_NAME = "headingAccuracyInDecimalDegrees";

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();

			try {
				persistence.begin();

				// check user is logged in
				String customer = request.getParameter(CUSTOMER_REQUEST_PARAMETER);
				String username = request.getParameter(USERNAME_REQUEST_PARAMETER);
				String sid = request.getParameter(SESSION_REQUEST_PARAMETER);

				StringBuilder principal = new StringBuilder(64);
				principal.append(customer).append('/').append(username);
				// throws an IllegalStateException if the user doesn't exist -
				// ends up as 500 error
				User user = WebUtil.processUserPrincipalForRequest(request, principal.toString(), false);
				if (user == null) {
					throw new SessionEndedException();
				}
				persistence.setUser(user);

				String hashedPassword = sid;

				if (sid == null) {
					// can't get values until we are logged in
					throw new Exception("Can't retrieve list until you have a valid session.");
				}
				assertLoggedIn(persistence, user, hashedPassword); // throws if
																	// not
																	// logged in

				String getType = request.getParameter(GET_TYPE_REQUEST_PARAMETER);
				
				if (GET_TYPE_DOMAIN_LIST.equals(getType)) {
					// data request
					String moduleName = request.getParameter(MODULE_REQUEST_PARAMETER);
					String documentName = request.getParameter(DOCUMENT_REQUEST_PARAMETER);
					StringBuilder sb = new StringBuilder();

					if (moduleName != null && documentName != null) {
						DocumentQuery q = persistence.newDocumentQuery(moduleName, documentName);
						q.addBoundOrdering(Bean.BIZ_KEY);

						List<Bean> beans = q.projectedResults();
						int i = 0;
						
						sb.append("[");
						for (Bean bean : beans) {
							String value = (String) Binder.get(bean, Bean.BIZ_KEY);
							String bizId = (String) Binder.get(bean, Bean.DOCUMENT_ID);
							sb.append((i++ == 0 ? "" : ",")).append("{\"v\":\"").append(bizId).append("\",\"d\":\"").append(value).append("\"}");
						}
						sb.append("]");
						pw.append(sb.toString());
					}
				} else if(GET_TYPE_USER_POSITION_LIST.equals(getType)){
//					String include = request.getParameter("userIncludeList");
					
					/*
					@SuppressWarnings("unchecked")
					Map<String, Object> values = (Map<String, Object>) JSONUtil.unmarshall(user, include);
	
					if(values.isEmpty()){
						//get all user status
					} else {
						//filter for specific users
					}
					*/
					
					DocumentQuery q = persistence.newDocumentQuery(TRACK_MODULE_NAME, TRACK_DOCUMENT_NAME);
					q.addAggregateProjection(AggregateFunction.Max, TRACK_TIMESTAMP_PROPERTY_NAME, "MaxTimestamp");
					q.addBoundProjection(TRACK_USER_PROPERTY_NAME);
					q.addBoundGrouping(TRACK_USER_PROPERTY_NAME);
					q.addBoundOrdering(Binder.createCompoundBinding(TRACK_USER_PROPERTY_NAME, Bean.BIZ_KEY));
					
					//exclude the current user
					q.getFilter().addNotEquals(Binder.createCompoundBinding(TRACK_USER_PROPERTY_NAME,  Bean.DOCUMENT_ID), user.getId());
					
					//add in filter for user list
					List<Map<String, Object>> userPositions = new ArrayList<>();
					
					//get max timestamp for each user, and then find associated track
					List<Bean> beans = q.projectedResults();
					for(Bean bean: beans){
						
						Bean u = (Bean) Binder.get(bean, TRACK_USER_PROPERTY_NAME);
						Timestamp t = (Timestamp) Binder.get(bean, "MaxTimestamp");
						//get the track associated with this timestamp and user
						DocumentQuery uq = persistence.newDocumentQuery(TRACK_MODULE_NAME, TRACK_DOCUMENT_NAME);
						uq.getFilter().addEquals(TRACK_USER_PROPERTY_NAME, u);
						uq.getFilter().addEquals(TRACK_TIMESTAMP_PROPERTY_NAME, t);
						
						List<Bean> tracks = uq.projectedResults();
						if(!tracks.isEmpty()){
							
							//Construct the user position report
							@SuppressWarnings("unchecked")
							List<Bean> vertices = (List<Bean>) Binder.get(tracks.get(0),"vertices");
							Bean vertex = vertices.get(vertices.size()-1); //get last vertex of whatever geometry they laid down - their last position
							
							Decimal10 latitudeInDecimalDegrees = (Decimal10) Binder.get(vertex, "latitudeInDecimalDegrees");
							Decimal10 longitudeInDecimalDegrees = (Decimal10) Binder.get(vertex, "longitudeInDecimalDegrees");
							Timestamp timestamp = (Timestamp) Binder.get(vertex, "timestamp");
							String address = (String) Binder.get(vertex, "address");
							String city = (String) Binder.get(vertex, "city");
							Integer speedInMetresPerSecond = (Integer) Binder.get(vertex, "speedInMetresPerSecond");
							Decimal2 trueHeadingInDecimalDegrees = (Decimal2) Binder.get(vertex, "trueHeadingInDecimalDegrees");
							
							String userContactName = (String) Binder.get(u, "contact.name");
							
							Map<String, Object> userPosition = new TreeMap<>();
							userPosition.put("userId", u.getBizId());
							userPosition.put("userContactName", userContactName);
							userPosition.put("latitudeInDecimalDegrees", latitudeInDecimalDegrees);
							userPosition.put("longitudeInDecimalDegrees", longitudeInDecimalDegrees);
							userPosition.put("timestamp", timestamp);
							userPosition.put("address", address);
							userPosition.put("city", city);
							userPosition.put("speedInMetresPerSecond", speedInMetresPerSecond);
							userPosition.put("trueHeadingInDecimalDegrees", trueHeadingInDecimalDegrees);
							
							//add to list of results
							userPositions.add(userPosition);
						}
					}
					
					//and return
					Map<String, Object> result = new TreeMap<>();
					result.put("data", userPositions);
					String jsonString = JSON.marshall(persistence.getUser().getCustomer(), result, null);
					System.out.println(jsonString);
					pw.append(jsonString);
				}

			} catch (@SuppressWarnings("unused") MetaDataException e) {
				// do nothing -just wait
			} finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		try (PrintWriter pw = response.getWriter()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				try {
					persistence.begin();

					String customer = request.getParameter(CUSTOMER_REQUEST_PARAMETER);
					String username = request.getParameter(USERNAME_REQUEST_PARAMETER);
					String password = request.getParameter(PASSWORD_REQUEST_PARAMETER);
					String hashedPassword = request.getParameter(SESSION_REQUEST_PARAMETER);
					String action = request.getParameter(POST_ACTION_PARAMETER);

					StringBuilder principal = new StringBuilder(64);
					principal.append(customer).append('/').append(username);

					// throws an IllegalStateException if the user doesn't exist
					// - ends up as 500 error
					User user = WebUtil.processUserPrincipalForRequest(request, principal.toString(), false);
					if (user == null) {
						throw new SessionEndedException();
					}
					persistence.setUser(user);

					StringBuilder result = new StringBuilder(128);

					if (LOGIN_ACTION_NAME.equals(action)) {
						login(result, persistence, user, password, hashedPassword);
					} else if(TRACK_ACTION_NAME.equals(action)){
						track(request, result, persistence, user, hashedPassword);
					}

					pw.print(result.toString());
				} catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			} catch (Throwable t) {
				t.printStackTrace();
				persistence.rollback();
				response.sendError(500); // flag an error
			} finally {
				if (persistence != null) {
					persistence.commit(true);
				}
			}
		}
	}

	private static void login(StringBuilder response, AbstractPersistence persistence, User user, String password, String sid) throws Exception {
		String hashedPassword = sid;

		if (sid == null || sid.length()==0) {
			MessageDigest md = MessageDigest.getInstance(UtilImpl.PASSWORD_HASHING_ALGORITHM);
			Base64 base64Codec = new Base64();
			hashedPassword = new String(base64Codec.encode(md.digest(password.getBytes())));
		}
		assertLoggedIn(persistence, user, hashedPassword); // throws if not
															// logged in
		
		response.append("{\"sid\":\"").append(hashedPassword).append("\"}");
	}

	private static void track(HttpServletRequest request, StringBuilder response, AbstractPersistence persistence, User user, String hashedPassword) throws Exception {
		assertLoggedIn(persistence, user, hashedPassword);

		Customer customer = user.getCustomer();
		String customerName = customer.getName();

		String result = request.getParameter("result");
		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) JSON.unmarshall(user, result);

		Module trackModule = customer.getModule(TRACK_MODULE_NAME);
		Document trackDoc = trackModule.getDocument(customer, TRACK_DOCUMENT_NAME);
		Document geoLocationDoc = trackModule.getDocument(customer, GEOLOCATION_DOCUMENT_NAME);
		Document userDoc = trackModule.getDocument(customer, ADMIN_USER_DOCUMENT_NAME);

		// Create geometry object
		PersistentBean track = trackDoc.newInstance(user);
		
		//get admin user from database
		Bean adminUserBean = persistence.retrieve(userDoc, user.getId(), false);
		Binder.set(track, TRACK_USER_PROPERTY_NAME, adminUserBean);
		
		//stuff type
		String type = UtilImpl.processStringValue((String) values.get("type"));
		if(type!=null){
			Binder.convertAndSet(track, TRACK_TYPE_PROPERTY_NAME, type);
		} else {
			//default
			Binder.convertAndSet(track, TRACK_TYPE_PROPERTY_NAME, TRACK_TYPE_POINT_VALUE);
		}

		// stuff description
		String description = UtilImpl.processStringValue((String) values.get("description"));
		if (description != null) {
			Binder.set(track, DESCRIPTION_PROPERTY_NAME, description);
		}

		// stuff mode
		String mode = UtilImpl.processStringValue((String) values.get("mode"));
		if (mode != null) {
			Binder.convertAndSet(track, TRACK_MODE_PROPERTY_NAME, mode);
		}
		
		// stuff discriminator
		String discriminatorId = UtilImpl.processStringValue((String) values.get("discriminator"));
		if (discriminatorId != null) {
			// lookup the discriminator by Id
			DocumentQuery q = persistence.newDocumentQuery(TRACK_MODULE_NAME, DISCRIMINATOR_DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, discriminatorId);

			List<PersistentBean> beans = q.projectedResults();
			if (!beans.isEmpty()) {
				Binder.set(track, TRACK_DISCRIMINATOR_PROPERTY_NAME, beans.get(0));
			}
		}

		String image = (String) values.get("image");
		if ((image != null) && (!image.isEmpty())) {
			if (image.startsWith("data:image/jpeg;base64,")) {
				image = image.substring(23);
			}
			byte[] bytes = Base64.decodeBase64(image.getBytes());

			// Rotate the image if required
			Number rotation = (Number) values.get("imageRotation");
			if (rotation != null) {
				double degrees = rotation.doubleValue();
				if (degrees != 0.0) {
					BufferedImage bi = ImageIO.read(new ByteArrayInputStream(bytes));
					AffineTransform at = new AffineTransform();
					at.translate(-(bi.getWidth() - bi.getHeight()) / 2, (bi.getWidth() - bi.getHeight()) / 2);
					at.rotate(Math.toRadians(degrees), bi.getWidth() / 2, bi.getHeight() / 2);
					AffineTransformOp opRotated = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR);
					bi = opRotated.filter(bi, null);
					ByteArrayOutputStream baos = new ByteArrayOutputStream(bytes.length);
					ImageIO.write(bi, "JPG", baos);
					bytes = baos.toByteArray();
				}
			}

			try (ContentManager cm = EXT.newContentManager()) {
				AttachmentContent content = new AttachmentContent(customerName,
																	TRACK_MODULE_NAME,
																	TRACK_DOCUMENT_NAME,
																	null,
																	user.getId(),
																	track.getBizId(),
																	PHOTO_PROPERTY_NAME,
																	MimeType.jpeg,
																	bytes);
				content.setContentId((String) Binder.get(track, PHOTO_PROPERTY_NAME));
				cm.put(content);
				Binder.set(track, PHOTO_PROPERTY_NAME, content.getContentId());
			}
		}

		// Create vertices objects and geometry object
		GeometryFactory gf = new GeometryFactory();
		Geometry geometry = null;
		@SuppressWarnings("unchecked")
		List<Bean> vertices = (List<Bean>) Binder.get(track, VERTICES_PROPERTY_NAME);
		@SuppressWarnings("unchecked")
		List<Map<String, Object>> positions = (List<Map<String, Object>>) values.get("positions");

		List<Coordinate> coords = new ArrayList<>();
		if (positions != null) {
			
			for (Map<String, Object> position : positions) {
				PersistentBean vertex = geoLocationDoc.newInstance(user);
				Coordinate coordinate = populateVertex(vertex, position);
				coords.add(coordinate);
				
				// Add to the collection
				vertices.add(vertex);
				Binder.set(vertex, ChildBean.PARENT_NAME, track);
				
				//set timestamp of track to latest timestamp encountered
				Binder.set(track, TRACK_TIMESTAMP_PROPERTY_NAME, Binder.get(vertex, VERTEX_TIMESTAMP_PROPERTY_NAME));
			}
		}

		// Create geometry object
		if(TRACK_TYPE_POINT_VALUE.equals(type)){
			geometry = gf.createPoint(coords.get(0));
		} else if(TRACK_TYPE_POLYGON_VALUE.equals(type)){
			//check for closed and if not, close it
			Coordinate startCoord = coords.get(0);
			Coordinate endCoord = (positions == null) ? null : coords.get(positions.size()-1);
			if(startCoord.equals(endCoord)){
				//closed
			} else {
				//close the shape
				coords.add(startCoord);
			}
			Coordinate[] arr = coords.toArray(new Coordinate[coords.size()]);
			geometry = gf.createPolygon(arr);
		} else if(TRACK_TYPE_POLYLINE_VALUE.equals(type)){
			Coordinate[] arr = coords.toArray(new Coordinate[coords.size()]);
			geometry = gf.createLineString(arr);
		} else {
			//rob's a bogun
			throw new Exception("Can't resolve the type of geometry");
		}
		
		Binder.set(track, GEOMETRY_PROPERTY_NAME, geometry);

		// stuff hash
		String originalDocumentHash = UtilImpl.processStringValue((String) values.get("originalDocumentHash"));
		if (originalDocumentHash != null) {
			Binder.set(track, TRACK_INTEGRITY_HASH_PROPERTY_NAME, originalDocumentHash);
		}

		
		track = persistence.save(trackDoc, track);

		response.append("{}");
	}

	private static void assertLoggedIn(AbstractPersistence persistence, User user, String hashedPassword) throws Exception {
		BizQL bizQL = persistence.newBizQL("select bean from {admin.User} as bean " + "where bean.bizCustomer = :customer " + "and bean.userName = :username " + "and bean.password = :password");
		bizQL.putParameter(CUSTOMER_REQUEST_PARAMETER, user.getCustomer().getName());
		bizQL.putParameter(USERNAME_REQUEST_PARAMETER, user.getName());
		bizQL.putParameter(PASSWORD_REQUEST_PARAMETER, hashedPassword);
		List<Bean> beans = bizQL.projectedResults();

		if (beans.size() != 1) {
			throw new SecurityException("trackmate", user.getName());
		}
	}

	@SuppressWarnings("unchecked")
	private static Coordinate populateVertex(PersistentBean vertex, Map<String, Object> position) throws Exception {
		double lng = 0.0;
		double lat = 0.0;

		Map<String, Object> coords = (Map<String, Object>) position.get("coords");
		// Note - This can be an integer (especially in ripple simulator), in
		// which case
		// it's doubled up as the heading is also in the GPS coords object,
		// so there's no need to keep it here
		Object headingVariant = position.get("heading");
		Map<String, Object> heading = null;
		if (headingVariant instanceof Map<?, ?>) {
			heading = (Map<String, Object>) headingVariant;
		}
		

		// Note - gps timestamp takes precedence over heading timestamp
		Number number = null;
		if (heading != null) {
			number = (Number) heading.get("timestamp");
			if (number != null) {
				long timestamp = number.longValue();
				if (timestamp >= 0) {
					Binder.set(vertex, VERTEX_TIMESTAMP_PROPERTY_NAME, new Timestamp(timestamp));
				}
			}
		}
		Object object = position.get("timestamp");
		if (object instanceof String) {
			number = Long.valueOf(new Timestamp((String) object).getTime());
		}
		else if (object instanceof Number) {
			number = (Number) object;
		}
		if (number != null) {
			long timestamp = number.longValue();
			if (timestamp >= 0) {
				Binder.set(vertex, VERTEX_TIMESTAMP_PROPERTY_NAME, new Timestamp(timestamp));
			}
		}

		// Fill in the gps coords
		number = (Number) coords.get("longitude");
		if (number != null) {
			lng = number.doubleValue();
			Binder.set(vertex, VERTEX_LONGITUDE_PROPERTY_NAME, new Decimal10(lng));
		}
		number = (Number) coords.get("latitude");
		if (number != null) {
			lat = number.doubleValue();
			Binder.set(vertex, VERTEX_LATITUDE_PROPERTY_NAME, new Decimal10(lat));
		}
		number = (Number) coords.get("accuracy");
		if (number != null) {
			int accuracy = number.intValue();
			if (accuracy >= 0) {
				Binder.set(vertex, VERTEX_POSITION_ACCURACY_PROPERTY_NAME, Integer.valueOf(accuracy));
			}
		}
		number = (Number) coords.get("altitude");
		if (number != null) {
			int altitude = number.intValue();
			if (altitude >= 0) {
				Binder.set(vertex, VERTEX_ALTITUDE_PROPERTY_NAME, Integer.valueOf(altitude));
			}
		}
		number = (Number) coords.get("altitudeAccuracy");
		if (number != null) {
			int accuracy = number.intValue();
			if (accuracy >= 0) {
				Binder.set(vertex, VERTEX_ALTITUDE_ACCURACY_PROPERTY_NAME, Integer.valueOf(accuracy));
			}
		}
		number = (Number) coords.get("speed");
		if (number != null) {
			int speed = number.intValue();
			if (speed >= 0) {
				Binder.set(vertex, VERTEX_SPEED_PROPERTY_NAME, Integer.valueOf(speed));
			}
		}

		// Fill in the reverse geocoded address
		String address = (String) position.get("address");
		Binder.set(vertex, ADDRESS_PROPERTY_NAME, UtilImpl.processStringValue(address));
		String city = (String) position.get("city");
		Binder.set(vertex, CITY_PROPERTY_NAME, UtilImpl.processStringValue(city));
		String state = (String) position.get("state");
		Binder.set(vertex, STATE_PROPERTY_NAME, UtilImpl.processStringValue(state));
		String postCode = (String) position.get("postCode");
		Binder.set(vertex, POST_CODE_PROPERTY_NAME, UtilImpl.processStringValue(postCode));
		String country = (String) position.get("country");
		Binder.set(vertex, COUNTRY_PROPERTY_NAME, UtilImpl.processStringValue(country));

		// Fill in the compass data
		// Compass true heading takes precedence over the gps heading
		number = (Number) coords.get("heading");
		if (number != null) {
			double value = number.doubleValue();
			if (value >= 0) {
				Binder.set(vertex, VERTEX_TRUE_HEADING_PROPERTY_NAME, new Decimal2(value));
			}
		}
		if (heading != null) {
			number = (Number) heading.get("magneticHeading");
			if (number != null) {
				double value = number.doubleValue();
				if (value >= 0) {
					Binder.set(vertex, VERTEX_MAGNETIC_HEADING_PROPERTY_NAME, new Decimal2(value));
				}
			}
			number = (Number) heading.get("trueHeading");
			if (number != null) {
				double value = number.doubleValue();
				if (value >= 0) {
					Binder.set(vertex, VERTEX_TRUE_HEADING_PROPERTY_NAME, new Decimal2(value));
				}
			}
			number = (Number) heading.get("headingAccuracy");
			if (number != null) {
				double value = number.doubleValue();
				if (value >= 0) {
					Binder.set(vertex, VERTEX_HEADING_ACCURACY_PROPERTY_NAME, new Decimal2(value));
				}
			}
		}

		return new Coordinate(lng, lat);
	}
}
