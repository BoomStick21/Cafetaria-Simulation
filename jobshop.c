/* External definitions for job-shop model. */

#include "simlib.h"		/* Required for use of simlib.c. */
#include <time.h>
#include <stdlib.h>

#define EVENT_ARRIVAL         1	/* Event type for arrival of a job to the
				   system. */
#define EVENT_DEPARTURE       2	/* Event type for departure of a job from a
				   particular station. */
#define EVENT_END_SIMULATION  3	/* Event type for end of the simulation. */
#define STREAM_INTERARRIVAL   1	/* Random-number stream for interarrivals. */
#define STREAM_GROUP          2
#define STREAM_ROUTE          3
#define STREAM_ST_HOT         4
#define STREAM_ST_SANDWICH    5
#define STREAM_ST_DRINK       6
#define STREAM_TOTAL_ACT      7
#define MAX_NUM_STATIONS      4	/* Maximum number of stations. */
#define MAX_NUM_JOB_TYPES     3	/* Maximum number of job types. */
#define MAX_NUM_CUSTOMERS      4 /* Maximum number of customers in a group. */

/* Declare non-simlib global variables. */

int num_stations, num_job_types, num_cust_types, i, j, num_customers, depart_job_type, depart_task, num_machines[MAX_NUM_STATIONS + 1],
  num_tasks[MAX_NUM_JOB_TYPES + 1],
  route[MAX_NUM_JOB_TYPES + 1][MAX_NUM_STATIONS + 1], num_machines_busy[MAX_NUM_STATIONS + 1], job_type[MAX_NUM_CUSTOMERS], task[MAX_NUM_CUSTOMERS];
double mean_interarrival, length_simulation, prob_distrib_job_type[26], prob_distrib_customer[26],
  min_service[MAX_NUM_JOB_TYPES + 1][MAX_NUM_STATIONS + 1], max_service[MAX_NUM_JOB_TYPES + 1][MAX_NUM_STATIONS + 1];
FILE *infile, *outfile;

int random(int max) {
  return rand() % max + 1;
}

int random_with_prob(double prob_distrib[], int stream ) {
  int i = 1;
  double u;

  u = random(100);
  
  if (u <= prob_distrib[i])
  {
    return i;
  }

  int min_val = 0;
  int max_val = 0;
  for (int i = 2; i <= stream; ++i)
  {
    min_val = min_val + prob_distrib[i-1];
    max_val = min_val + prob_distrib[i];
    if (u > min_val && u <= max_val)
    {
      return i;
    }
  }
}

int get_stream_station(int i) {
  switch (i) {
    case 1:
      return STREAM_ST_HOT;
      break;
    case 2:
      return STREAM_ST_SANDWICH;
      break;
    case 3:
      return STREAM_ST_DRINK;
      break;
    case 4:
      return STREAM_TOTAL_ACT;
      break;
  }
}

void serve(int customer) {
  int station;
  /* Determine the station from the route matrix. */

  station = route[job_type[customer]][task[customer]];

  /* Check to see whether all machines in this station are busy. */

  if (num_machines_busy[station] == num_machines[station])
  {

    /* All machines in this station are busy, so place the arriving job at
       the end of the appropriate queue. Note that the following data are
       stored in the record for each job:
       1. Time of arrival to this station.
       2. Job type.
       3. Current task number. */

    transfer[1] = sim_time;
    transfer[2] = job_type[customer];
    transfer[3] = task[customer];
    list_file (LAST, station);
  }

  else
  {

    /* A machine in this station is idle, so start service on the arriving
       job (which has a delay of zero). */

    sampst (0.0, station);  /* For station. */
    sampst (0.0, num_stations + job_type[customer]); /* For job type. */
    ++num_machines_busy[station];
    timest ((double) num_machines_busy[station], station);

    /* Schedule a service completion.  Note defining attributes beyond the
       first two for the event record before invoking event_schedule. */

    transfer[3] = job_type[customer];
    transfer[4] = task[customer];
    transfer[5] = customer;
    event_schedule (sim_time + uniform (min_service[job_type[customer]][task[customer]], max_service[job_type[customer]][task[customer]], get_stream_station(station)), EVENT_DEPARTURE);
  }
}

void
arrive (int new_job)    /* Function to serve as both an arrival event of a job
           to the system, as well as the non-event of a job's
           arriving to a subsequent station along its
           route. */
{

  /* If this is a new arrival to the system, generate the time of the next
     arrival and determine the job type and task number of the arriving
     job. */

  if (new_job == 1)
  {
    event_schedule (sim_time + expon (mean_interarrival, STREAM_INTERARRIVAL), EVENT_ARRIVAL);
    num_customers = random_integer(prob_distrib_customer, STREAM_GROUP);
    for (int i = 1; i <= num_customers; ++i)
    {
      job_type[i] =  random_integer(prob_distrib_job_type, STREAM_ROUTE);       
      task[i] = 1;
    }
  }

  for (int i = 1; i <= num_customers; ++i)
  {
    serve(i);
  }
}


void
depart ()			/* Event function for departure of a job from a particular
				   station. */
{
  int station, job_type_queue, task_queue, customer;

  /* Determine the station from which the job is departing. */

  customer = transfer[5];
  job_type[customer] = transfer[3];
  task[customer] = transfer[4];
  station = route[job_type[customer]][task[customer]];

  /* Check to see whether the queue for this station is empty. */

  if (list_size[station] == 0)
    {

      /* The queue for this station is empty, so make a machine in this
         station idle. */

      --num_machines_busy[station];
      timest ((double) num_machines_busy[station], station);
    }

  else
    {

      /* The queue is nonempty, so start service on first job in queue. */

      list_remove (FIRST, station);

      /* Tally this delay for this station. */

      sampst (sim_time - transfer[1], station);

      /* Tally this same delay for this job type. */

      job_type_queue = transfer[2];
      task_queue = transfer[3];
      sampst (sim_time - transfer[1], num_stations + job_type_queue);

      /* Schedule end of service for this job at this station.  Note defining
         attributes beyond the first two for the event record before invoking
         event_schedule. */

      transfer[3] = job_type_queue;
      transfer[4] = task_queue;
      transfer[5] = customer;
      event_schedule (sim_time + uniform (min_service[job_type[customer]][task[customer]], max_service[job_type[customer]][task[customer]], get_stream_station(station)), EVENT_DEPARTURE);
    }

  /* If the current departing job has one or more tasks yet to be done, send
     the job to the next station on its route. */

  if (task[customer] < num_tasks[job_type[customer]])
    {
      task[customer] = task[customer] + 1;
      serve(customer);
    }
}


void
report (void)			/* Report generator function. */
{
  int i;
  double overall_avg_job_tot_delay, avg_job_tot_delay, sum_probs;

  /* Compute the average total delay in queue for each job type and the
     overall average job total delay. */

  fprintf (outfile, "\n\n\n\nJob type     Average total delay in queue");
  overall_avg_job_tot_delay = 0.0;
  sum_probs = 0.0;
  for (i = 1; i <= num_job_types; ++i)
    {
      avg_job_tot_delay = sampst (0.0, -(num_stations + i)) * num_tasks[i];
      fprintf (outfile, "\n\n%4d%27.3f", i, avg_job_tot_delay);
      overall_avg_job_tot_delay += (prob_distrib_job_type[i] - sum_probs) * avg_job_tot_delay;
      sum_probs = prob_distrib_job_type[i];
    }
  fprintf (outfile, "\n\nOverall average job total delay =%10.3f\n", overall_avg_job_tot_delay);

  /* Compute the average number in queue, the average utilization, and the
     average delay in queue for each station. */

  fprintf (outfile, "\n\n\n Work      Average number      Maximum number      Average       Average delay       Maximum delay");
  fprintf (outfile, "\nstation       in queue            in queue       utilization        in queue           in queue");
  for (j = 1; j <= num_stations; ++j)
  {
    sampst(0.0, -j);
    float avg_delay = transfer[1];
    float max_delay = transfer[3];

    filest(j);
    float avg_queue = transfer[1];
    float max_queue = transfer[2];
    if (max_queue < 0)
    { 
      max_queue = 0;
    }
    if (j == 4)
    {
      max_queue = max_queue*2;
    }

    fprintf (outfile, "\n\n%4d%17.3f%21.3f%17.3f%17.3f%21.3f", j, avg_queue, max_queue, timest (0.0, -j) / num_machines[j], avg_delay, max_delay);
  }
}

int
main ()				/* Main function. */
{
  /* Open input and output files. */

  infile = fopen ("jobshop.in", "r");
  outfile = fopen ("jobshop.out", "w");

  /* Read input parameters. */

  fscanf (infile, "%d %d %d %lg %lg", &num_stations, &num_job_types, &num_cust_types, &mean_interarrival, &length_simulation);
  for (j = 1; j <= num_stations; ++j)
    fscanf (infile, "%d", &num_machines[j]);
  for (i = 1; i <= num_job_types; ++i)
    fscanf (infile, "%d", &num_tasks[i]);
  for (i = 1; i <= num_job_types; ++i)
    {
      for (j = 1; j <= num_tasks[i]; ++j)
	     fscanf (infile, "%d", &route[i][j]);
      for (j = 1; j <= num_tasks[i]; ++j)
	     fscanf (infile, "%lg", &min_service[i][j]);
      for (j = 1; j <= num_tasks[i]; ++j)
       fscanf (infile, "%lg", &max_service[i][j]);
    }
  for (i = 1; i <= num_cust_types; ++i)
    fscanf (infile, "%lg", &prob_distrib_customer[i]); 
  for (i = 1; i <= num_job_types; ++i)
    fscanf (infile, "%lg", &prob_distrib_job_type[i]);

  /* Write report heading and input parameters. */

  fprintf (outfile, "Cafetaria model\n\n");
  fprintf (outfile, "Number of work stations%21d\n\n", num_stations);
  fprintf (outfile, "Number of machines in each station     ");
  for (j = 1; j <= num_stations; ++j)
    fprintf (outfile, "%5d", num_machines[j]);
  fprintf (outfile, "\n\nNumber of job types%25d\n\n", num_job_types);
  fprintf (outfile, "Number of tasks for each job type      ");
  for (i = 1; i <= num_job_types; ++i)
    fprintf (outfile, "%5d", num_tasks[i]);
  fprintf (outfile, "\n\nDistribution function of job types  ");
  for (i = 1; i <= num_job_types; ++i)
    fprintf (outfile, "%8.3f", prob_distrib_job_type[i]);
  fprintf (outfile, "\n\nMean interarrival time of jobs%14.2f seconds\n\n", mean_interarrival);
  fprintf (outfile, "Length of the simulation%20.1f hours \n\n\n", length_simulation);
  fprintf (outfile, "Job type     Work stations on route");
  for (i = 1; i <= num_job_types; ++i)
    {
      fprintf (outfile, "\n\n%4d        ", i);
      for (j = 1; j <= num_tasks[i]; ++j)
	fprintf (outfile, "%5d", route[i][j]);
    }
  fprintf (outfile, "\n\n\nJob type     ");
  fprintf (outfile, "Service time (in seconds) for successive tasks");
  for (i = 1; i <= num_job_types; ++i)
    {
      fprintf (outfile, "\n\n%4d    ", i);
      for (j = 1; j <= num_tasks[i]; ++j)
	fprintf (outfile, "%9.2f-%.2f", min_service[i][j], max_service[i][j]);
    }

  /* Initialize all machines in all stations to the idle state. */

  for (j = 1; j <= num_stations; ++j)
    num_machines_busy[j] = 0;

  /* Initialize simlib */

  init_simlib ();

  /* Initialize randomizer */

  srand(time(NULL));

  /* Set maxatr = max(maximum number of attributes per record, 4) */

  maxatr = 4;			/* NEVER SET maxatr TO BE SMALLER THAN 4. */

  /* Schedule the arrival of the first job. */

  event_schedule (expon (mean_interarrival, STREAM_INTERARRIVAL), EVENT_ARRIVAL);

  /* Schedule the end of the simulation.  (This is needed for consistency of
     units.) */

  event_schedule (3600*length_simulation, EVENT_END_SIMULATION);

  /* Run the simulation until it terminates after an end-simulation event
     (type EVENT_END_SIMULATION) occurs. */

  do
    {

      /* Determine the next event. */

      timing ();

      /* Invoke the appropriate event function. */

      switch (next_event_type)
	{
	case EVENT_ARRIVAL:
  {
    arrive (1);
  	break;
  }
	case EVENT_DEPARTURE:
	  depart ();
	  break;
	case EVENT_END_SIMULATION:
	  report ();
	  break;
	}

      /* If the event just executed was not the end-simulation event (type
         EVENT_END_SIMULATION), continue simulating.  Otherwise, end the
         simulation. */

    }
  while (next_event_type != EVENT_END_SIMULATION);

  fclose (infile);
  fclose (outfile);

  return 0;
}
