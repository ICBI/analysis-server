package gov.nih.nci.caintegrator.analysis.server;

import gov.nih.nci.caintegrator.analysis.messaging.AnalysisRequest;
import gov.nih.nci.caintegrator.analysis.messaging.AnalysisResult;
import gov.nih.nci.caintegrator.analysis.messaging.CorrelationRequest;
import gov.nih.nci.caintegrator.analysis.messaging.CorrelationResult;
import gov.nih.nci.caintegrator.analysis.messaging.DataPoint;
import gov.nih.nci.caintegrator.analysis.messaging.ReporterInfo;
import gov.nih.nci.caintegrator.analysis.messaging.SampleGroup;
import gov.nih.nci.caintegrator.enumeration.AxisType;
import gov.nih.nci.caintegrator.enumeration.CorrelationType;
import gov.nih.nci.caintegrator.exceptions.AnalysisServerException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RserveException;

public class CorrelationTaskR extends AnalysisTaskR {

	private CorrelationResult result;
	
	private static Logger logger = Logger.getLogger(CorrelationTaskR.class);
	private CorrelationRequest corrRequest;

	
	public CorrelationTaskR(AnalysisRequest request) {
		super(request);
		
	}

	public CorrelationTaskR(AnalysisRequest request, boolean debugRcommands) {
		super(request, debugRcommands);
		
	}

	@Override
	public void run() {
		corrRequest = (CorrelationRequest) getRequest();
		result = new CorrelationResult(getRequest().getSessionId(), getRequest().getTaskId());
		logger.info(getExecutingThreadName() + " processing correlation request="
						+ corrRequest);
		
		List<DataPoint> computePoints = null;
	
		try {
		
			//Check to see if a reporter was passed in
			ReporterInfo reporter1 = corrRequest.getReporter1();
			ReporterInfo reporter2 = corrRequest.getReporter2();
			
			//List<String> restrictingSampleIds = corrRequest.getSampleIds();
			
			Double r = null;
			Map <String, DataPoint> vecMap = new HashMap<String, DataPoint>();
						
			if ((reporter1 != null) && (reporter2 != null)) {
			  //CASE 1:  correlation between two reporters
			  result.setGroup1Name(reporter1.getGeneSymbol() + "_" + reporter1.getReporterName());
			  result.setGroup2Name(reporter2.getGeneSymbol() + "_" + reporter2.getReporterName());
			  			 			 
			  setDataPoints(reporter1,reporter1.getSampleGroup(),vecMap, AxisType.X_AXIS, true);
			  setDataPoints(reporter2,reporter2.getSampleGroup(),vecMap, AxisType.Y_AXIS, false);  
			}
			else if ((reporter1!=null) && (reporter2==null)) {
			  //CASE2 : a reporter against a vector			 
			  result.setGroup1Name(reporter1.getGeneSymbol() + "_" + reporter1.getReporterName());
			  result.setGroup2Name(corrRequest.getVector2Name());
			  List<DataPoint> yPoints = corrRequest.getVector2();
			  setDataPoints(yPoints,vecMap, true);
			  setDataPoints(reporter1,reporter1.getSampleGroup(),vecMap, AxisType.X_AXIS, false);
			}
			else if ((reporter1==null) && (reporter2!=null) ) {	
				
			  result.setGroup1Name(corrRequest.getVector1Name());
			  result.setGroup2Name(reporter2.getGeneSymbol() + "_" + reporter2.getReporterName());					
			  List<DataPoint> xPoints = corrRequest.getVector1();
			  setDataPoints(xPoints,vecMap, true);
			  setDataPoints(reporter2,reporter2.getSampleGroup(),vecMap, AxisType.Y_AXIS, false);
			}
			else { 			  
			  result.setGroup1Name(corrRequest.getVector1Name());
			  result.setGroup2Name(corrRequest.getVector2Name());
			  List<DataPoint> xPoints = corrRequest.getVector1();
			  setDataPoints(xPoints, vecMap, true);
			  
			  List<DataPoint> yPoints = corrRequest.getVector2();
			  setDataPoints(yPoints, vecMap, false);
			}		
			
			computePoints = getComputePoints(vecMap.values());
			r = computeCorrelationCofficient(computePoints);
			
			result.setCorrelationValue(r);			
			result.setDataPoints(computePoints);
		}
		catch (AnalysisServerException asex) {
		    asex.setFailedRequest(corrRequest);
	        setException(asex);
	        logger.error("Caught AnalysisServerException");
	        logStackTrace(logger, asex);
	        return;  
		}
		catch (Exception ex) {
			AnalysisServerException asex = new AnalysisServerException(
			"Internal Error. Caught Exception in CorrelationTaskR exClass=" + ex.getClass() + " msg=" + ex.getMessage());
	        asex.setFailedRequest(corrRequest);
	        setException(asex);
	        logger.error("Caught Exception in CorrelationTaskR");	       
	        logStackTrace(logger, ex);
	        return;  
		}
	}

	private List<DataPoint> getComputePoints(Collection<DataPoint> points) {
		List<DataPoint> computePoints = new ArrayList<DataPoint>();
		for (DataPoint point: points) {
	      if ((point.getX()!=null) &&(point.getY()!=null)) {
	        computePoints.add(point);
	      }
		}
		return computePoints;
	}

	/**
	 * Compute the correlation coefficient based on the specified compute points.
	 * @param computePoints
	 * @return
	 * @throws AnalysisServerException
	 */
	private Double computeCorrelationCofficient(List<DataPoint> computePoints) throws AnalysisServerException {
		String v1str = "v1 <- c(" + getDataString(computePoints, true,false);
		  String v2str = "v2 <- c(" + getDataString(computePoints,false,true);
		  Double r = null;
		  String cmd;
		  		  
		  doRvoidEval(v1str);
		  doRvoidEval(v2str);
		  if (corrRequest.getCorrelationType() == CorrelationType.PEARSON) {
		    cmd = "r <- correlation(v1,v2,\"pearson\")";
		    
		    try {
		    	r = doREval(cmd).asDouble();
			} catch (REXPMismatchException ex) {
				logger.error("computeCorrelationCoefficient threw REXPMismatchException when executing command=" + cmd);
				logStackTrace(logger, ex);
				throw new AnalysisServerException("Internal Error. command=" + cmd);
			}
		    
		    
		  }
		  else if (corrRequest.getCorrelationType() == CorrelationType.SPEARMAN) {
		    cmd = "r <- correlation(v1,v2, \"spearman\")";
		    try {
		    	r = doREval(cmd).asDouble();
			} catch (REXPMismatchException ex) {
				logger.error("computeCorrelationCoefficient threw REXPMismatchException when executing command=" + cmd);
				logStackTrace(logger, ex);
				throw new AnalysisServerException("Internal Error. command=" + cmd);
			}
		  }
		  
		  return r;
	}
	
	/**
	 * Load the pontMap with the points
	 * @param points
	 * @param pointMap
	 * @param createPoints
	 */
	private void setDataPoints(List<DataPoint> points, Map<String, DataPoint> pointMap, boolean createPoints) {
	  String id;
	  DataPoint dp;
	
	  
	  if (points == null) {
	    logger.error("setDataPoints passed null paramter points");
	    return;
	  }
	  
	  
	  for (DataPoint point : points) {	
		  	  
		if (point == null) {
		  logger.warn("Found null point in points list. Skipping ...");
		  continue;
		}
		  
		id = point.getId();
		dp = pointMap.get(id);
		
		if (dp == null)  { 
		  if (createPoints) {
		    pointMap.put(id, point);
		  }
		  else {
			logger.warn("pointMap does not contain an entry with id=" + id + " skipping point...");
		  }
		}
		else {
		  if (dp.getX() == null) dp.setX(point.getX());
		  if (dp.getY() == null) dp.setY(point.getY());
		  //if (dp.getZ() == null) dp.setZ(point.getZ());
		}
	  }
	}

	/**
	 * Load the point map with the sample information from the binary file for the reporter specified.
	 * @param reporter
	 * @param sampleIds
	 * @param pointMap
	 * @param axis
	 * @param createNewPoints
	 * @throws AnalysisServerException
	 */
	private void setDataPoints(ReporterInfo reporter, SampleGroup sampleIds, Map<String, DataPoint> pointMap, AxisType axis, boolean createNewPoints) throws AnalysisServerException{
		
		try {
			
			setDataFile(reporter.getDataFileName());
		    String cmd;
			
			if ((sampleIds != null)&&(!sampleIds.isEmpty())) {
			  String sampleIdscmd = getRgroupCmd("sampleIds", sampleIds);
			  doRvoidEval(sampleIdscmd);
			
			  cmd = "SM <- getSubmatrix.onegrp(dataMatrix, sampleIds)";
			  doRvoidEval(cmd);			  
			}
			else {
			  //use the entire data matrix without restricting on samples
			  cmd = "SM <- dataMatrix";
			  doRvoidEval(cmd);
			}
			
			
			cmd = "RM <- getSubmatrix.rep(SM,\"" + reporter.getReporterName() + "\")";
			double[] vec =  doREval(cmd).asDoubles();
			//need to make sure that the vectors are in the same order wrt sample ids
			cmd = "RMlabels <- dimnames(RM)[[1]]";
			//Vector RM_ids = doREval(cmd).asVector();
			String[] RM_ids = doREval(cmd).asStrings();
			
		    if (RM_ids == null) {
		      throw new AnalysisServerException("No expression data found for reporter " + reporter.getReporterName() + " in data file=" + reporter.getDataFileName() + ".");
		    }
			
			DataPoint point;
			String id;
			//REXP exp;
			for (int i=0; i < RM_ids.length; i++) {
			  //exp = (REXP) RM_ids.get(i);
			  //id = exp.asString();
			  id = RM_ids[i];
			  
			  //logger.info("Adding data point with id=" + id);
			  
			  point = pointMap.get(id);
			  if (point == null) {
				if (createNewPoints) {
			      point = new DataPoint(id);
			      pointMap.put(id, point);
				}
				else {
				  logger.warn("Could not find id=" + id + " in point map reporter=" + reporter.getReporterName() + " skipping point..");
				  continue;
				}
			  }
			   
			  if (axis == AxisType.X_AXIS) {
			    point.setX(vec[i]);
			  }
			  else if (axis == AxisType.Y_AXIS) {
			    point.setY(vec[i]);
			  }
			  else if (axis == AxisType.Z_AXIS) {
			    point.setZ(vec[i]);
			  }
			}
		
		} 
		catch (Exception ex2) {
			logger.error("Caught exception in setDataPoints method for reporter=" + reporter);
			logStackTrace(logger, ex2);
		}
	}
	
	@Override
	public void cleanUp() {
		try {
			setRComputeConnection(null);
		} catch (AnalysisServerException e) {
			logger.error("Error in cleanUp method.");
			logStackTrace(logger, e);
			setException(e);
		}
	}

	@Override
	public AnalysisResult getResult() {
		return result;
	}
	
	/**
	 * 
	 * @param points
	 * @param useX
	 * @param useY
	 * @return Quoted string for use in R command
	 */
	private String getDataString(List<DataPoint> points, boolean useX, boolean useY) {
	  StringBuffer sb = new StringBuffer();
	  DataPoint point;
	  boolean pointAppended = false;
	  for (Iterator i=points.iterator(); i.hasNext(); ) {
		 point = (DataPoint) i.next();
		 
		 if ((point.getX()!=null)&&(point.getY()!=null)) {
			 if (pointAppended) {
		       sb.append(",");
			 }
			 if (useX) {
			   sb.append("\"").append(point.getX()).append("\"");
			 } 
			 
			 
			 if (useY) {
			   sb.append("\"").append(point.getY()).append("\"");
			 }
			 pointAppended = true;
		 }
	  }
	  sb.append(")");
	  return sb.toString();
      
	}
	  
}
