
/* **********************************************************************
   * Cost version 2.0, written by Fredric L. Rice.                      *
   *                                                                    *
   * Copyright by Fredric L. Rice, May, 1991. All rights reserved.      *
   * Author access: 1:102/901.0                                         *
   *                                                                    *
   ********************************************************************** */

#include <alloc.h>
#include <ctype.h>
#include <conio.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* **********************************************************************
   * Define various macros that will be needed.                         *
   *                                                                    *
   ********************************************************************** */

#define skipspace(s)    while (isspace(*s))  ++(s)

/* **********************************************************************
   * Define the global constants that will be used.                     *
   *                                                                    *
   ********************************************************************** */

#define TRUE            1
#define FALSE           0
#define The_Version     "2.01"

/* **********************************************************************
   * Define any data we need.                                           *
   *                                                                    *
   ********************************************************************** */

    static FILE *configuration;

/* **********************************************************************
   * Define the data structure.                                         *
   *                                                                    *
   * We maintain a linked list.                                         *
   *                                                                    *
   ********************************************************************** */

    static struct Config_Entry {
        char zone_local;                /* Zone number. 0 if local.     */
        short area_code;                /* The area code of this block. */
        short exchange;                 /* The exchange code.           */
        struct Config_Entry *next;      /* Point to the next one        */
    } *ce_first, *ce_last, *ce_test;    /* Create three pointers to it. */

    static char current_zone;
    static short current_area_code;

/* **********************************************************************
   * Define a data structure that keeps the cost information.           *
   *                                                                    *
   ********************************************************************** */

    static struct Cost_Entry {
        char zone;                      /* The zone number              */
        float first_minute;             /* Cost of the first minute     */
        float next_minute;              /* Cost of other minutes        */
        struct Cost_Entry *next;        /* Pointer to the next item.    */
    } *c_first, *c_last, *c_test;       /* Create three pointers to it. */

/* **********************************************************************
   * Define a data structure that keeps cost reduction information.     *
   *                                                                    *
   ********************************************************************** */

    static struct Cost_Reduction {
        short from;                     /* Starting 24 hour time        */
        short to;                       /* Endinf 24 hour time          */
        short percent;                  /* percentage.                  */
        struct Cost_Reduction *next;    /* Point to the next item.      */
    } *cr_first, *cr_last, *cr_test;    /* Create three pointers to it. */

    static short current_scan_zone = 1;
    static short current_network = 1;
    static short current_node = 1;

/* **********************************************************************
   * Set the offered string to uppercase.                               *
   *                                                                    *
   ********************************************************************** */

void ucase(char *this_record)
{
   while (*this_record) {
      if (*this_record > 0x60 && *this_record < 0x7b) {
         *this_record = *this_record - 32;
      }

      this_record++;
   }
}

/* **********************************************************************
   * Get all of the local data into the holding variables.              *
   *                                                                    *
   ********************************************************************** */

static void plug_local(char *this_line)
{
    this_line += 5;
    skipspace(this_line);
    current_zone = 0;
    current_area_code = atoi(this_line);

    if (current_area_code < 0 || current_area_code > 999) {

	(void)printf("Local area code in config file bad: %d\n",
	    current_area_code);

        (void)fcloseall();
        exit(12);
    }
}

/* **********************************************************************
   * Get all of the zone data into the holding variables.               *
   *                                                                    *
   ********************************************************************** */

static void plug_zone(char *this_line)
{
    this_line += 4;
    skipspace(this_line);
    current_zone = atoi(this_line);

    if (current_zone < 0 || current_zone > 99) {
        (void)printf("Zone in config file bad: %d\n", current_zone);
        (void)fcloseall();
        exit(12);
    }

    while (*this_line && *this_line != ' ') {
        this_line++;
    }

    if (*this_line) {
        skipspace(this_line);
	current_area_code = atoi(this_line);
	if (current_area_code < 0 || current_area_code > 999) {

	    (void)printf("Zone %d area code in config file bad: %d\n",
		current_zone, current_area_code);

            (void)fcloseall();
            exit(12);
        }
    }
    else {
	(void)printf("Zone area code in config file is missing exchange.\n");
        (void)fcloseall();
        exit(12);
    }
}

/* **********************************************************************
   * Get all of the cost data into the holding variables.               *
   *                                                                    *
   ********************************************************************** */

static void plug_cost(char *this_line)
{
    c_test = (struct Cost_Entry *)farmalloc(sizeof(struct Cost_Entry));

    if (c_test == (struct Cost_Entry *)NULL) {
        (void)printf("I ran out of memory allocating for cost information!\n");
        (void)fcloseall();
        exit(13);
    }

/*
    Store the information into the linked list
*/

    this_line += 9;
    skipspace(this_line);

    c_test->zone = atoi(this_line);

    if (c_test->zone < 0 || c_test->zone > 99) {
        (void)printf("Cost information has a bad zone number!\n");
        (void)fcloseall();
        exit(14);
    }

    while (*this_line && *this_line != ' ') {
        this_line++;
    }

    if (! *this_line) {
        (void)printf("Cost information is missing zone number!\n");
        (void)fcloseall();
        exit(14);
    }

    this_line++;
    skipspace(this_line);

    c_test->first_minute = atof(this_line);

    while (*this_line && *this_line != 'A') {
        this_line++;
    }

    if (! *this_line || strncmp(this_line, "AND", 3)) {
        (void)printf("Cost information is missing AND keyword!\n");
        (void)fcloseall();
        exit(14);
    }

    this_line += 3;
    skipspace(this_line);

    c_test->next_minute = atof(this_line);
    c_test->next = (struct Cost_Entry *)NULL;

    if (c_first == (struct Cost_Entry *)NULL) {
        c_first = c_test;
    }
    else {
        c_last->next = c_test;
    }

    c_last = c_test;
}

/* **********************************************************************
   * Get all of the from data into the holding variables.               *
   *                                                                    *
   ********************************************************************** */

static void plug_from(char *this_line)
{
    cr_test = (struct Cost_Reduction *)farmalloc(sizeof(struct Cost_Reduction));

    if (c_test == (struct Cost_Entry *)NULL) {
        (void)printf("I ran out of memory allocating for cost reduction information!\n");
        (void)fcloseall();
        exit(13);
    }

    this_line += 4;
    skipspace(this_line);
    cr_test->from = atoi(this_line);

    if (cr_test->from < 0 || cr_test->from > 24) {
        (void)printf("Cost reduction information is badly constructed!\n");
        (void)fcloseall();
        exit(15);
    }

    while (*this_line && *this_line != 'T') {
        this_line++;
    }

    if (! *this_line || strncmp(this_line, "TO", 2)) {
        (void)printf("Cost reduction entry is missing TO keyword!\n");
        (void)fcloseall();
        exit(15);
    }

    this_line += 2;
    skipspace(this_line);

    cr_test->to = atoi(this_line);

    if (cr_test->to < 0 || cr_test->to > 24) {
        (void)printf("Cost reduction information is badly constructed!\n");
        (void)fcloseall();
        exit(15);
    }

    while (*this_line && *this_line != ' ') {
        this_line++;
    }

    if (! *this_line) {
        (void)printf("Cost reduction information is missing percentage!\n");
        (void)fcloseall();
        exit(15);
    }

    cr_test->percent = atoi(this_line);

    if (cr_test->percent < 0 || cr_test->percent > 100) {
        (void)printf("Cost reduction information has strange percentage!\n");
        (void)fcloseall();
        exit(15);
    }

    cr_test->next = (struct Cost_Reduction *)NULL;

    if (cr_first == (struct Cost_Reduction *)NULL) {
        cr_first = cr_test;
    }
    else {
        cr_last->next = cr_test;
    }

    cr_last = cr_test;
}

/* **********************************************************************
   * Append the list of exchanges to the current area code.             *
   *                                                                    *
   ********************************************************************** */

static void add_entry(short exchange)
{
    ce_test = (struct Config_Entry *)farmalloc(sizeof(struct Config_Entry));

    if (ce_test == (struct Config_Entry *)NULL) {
        (void)printf("I ran out of memory allocating configuration information!\n");
        (void)fcloseall();
        exit(13);
    }

    ce_test->zone_local = current_zone;
    ce_test->area_code = current_area_code;
    ce_test->exchange = exchange;
    ce_test->next = (struct Config_Entry *)NULL;

    if (ce_first == (struct Config_Entry *)NULL) {
        ce_first = ce_test;
    }
    else {
        ce_last->next = ce_test;
    }

    ce_last = ce_test;
}

/* **********************************************************************
   * Go through the line of listed exchanges.                           *
   *                                                                    *
   ********************************************************************** */

static void append_exchange(char *this_line)
{
    while (*this_line) {
        add_entry(atoi(this_line));

	while (*this_line && *this_line != ' ') {
            this_line++;
	}

	if (*this_line) {
	    this_line++;
	}
    }
}

/* **********************************************************************
   * Get all of the information out of the configuration file.          *
   *                                                                    *
   ********************************************************************** */

static char extract_configuration(void)
{
    char record[201], *point;

    while (! feof(configuration)) {
        (void)fgets(record, 200, configuration);

        if (! feof(configuration)) {
            point = record;
            skipspace(point);

            if (strlen(point) > 2 && *point != ';') {
                ucase(point);

                if (! strncmp(point, "LOCAL", 5)) {
                    plug_local(point);
                }
                else if (! strncmp(point, "ZONE", 4)) {
                    plug_zone(point);
                }
                else if (! strncmp(point, "COST", 4)) {
                    plug_cost(point);
                }
		else if (! strncmp(point, "FROM", 4)) {
                    plug_from(point);
                }
                else {
                    append_exchange(point);
                }
            }
        }
    }

    (void)fclose(configuration);
    return(TRUE);
}

/* **********************************************************************
   * A badly formatted phone number was offered.                        *
   *                                                                    *
   ********************************************************************** */

static void give_bad_number(void)
{
    (void)printf("Enter phone number as: XXX-YYY-ZZZ\n");
    (void)printf("XXX = Area code\n");
    (void)printf("YYY = Exchange code\n");
    (void)printf("ZZZZ = Phone number\n");
    (void)fcloseall();
    exit(16);
}

/* **********************************************************************
   * Look for the specified phone number and report.                    *
   *                                                                    *
   ********************************************************************** */

static void scan_phone_number(char *this_one)
{
    unsigned short area_code;
    unsigned short exchange;
    unsigned short number;

    area_code = atoi(this_one);

    if (area_code < 100 || area_code > 999) {
        give_bad_number();
    }

    while (*this_one && *this_one != '-') {
        this_one++;
    }

    if (! *this_one) {
        give_bad_number();
    }

    this_one++;
    exchange = atoi(this_one);

    if (exchange < 100 || exchange > 999) {
        give_bad_number();
    }

    while (*this_one && *this_one != '-') {
        this_one++;
    }

    if (! *this_one) {
        give_bad_number();
    }

    this_one++;

    if (! *this_one) {
        give_bad_number();
    }

    number = atoi(this_one);

    clrscr();
    (void)printf("%d-%d-%d\n", area_code, exchange, number);

    ce_test = ce_first;

    while (ce_test) {
	if (ce_test->area_code == area_code && ce_test->exchange == exchange) {
	    if (ce_test->zone_local == 0) {
		(void)printf("That's a local phone number\n");
		return;
	    }

	    (void)printf("That's a zone %d call: \n");

	    c_test = c_first;

	    while (c_test) {
		if (c_test->zone == ce_test->zone_local) {

		    (void)printf("   %02f first minute, %02f for the next\n",
			c_test->first_minute, c_test->next_minute);

		    return;
		}

		c_test = c_test->next;
	    }

	    (void)printf("No cost information was provided.\n");
	    return;
	}

        ce_test = ce_test->next;
    }

    (void)printf("That's a long distance call. Reduced rate information:\n");

    cr_test = cr_first;

    while (cr_test) {
        (void)printf("From %02d until %02d, cost is %03d%% off\n",
            cr_test->from, cr_test->to, cr_test->percent);

        cr_test = cr_test->next;
    }
}

/* **********************************************************************
   * Check to see if this is a local or cheap zone. Report it if so.    *
   *                                                                    *
   ********************************************************************** */

static void check_entry(unsigned short area_code,
    unsigned short exchange,
    char *system_name)
{
    ce_test = ce_first;

    while (ce_test) {
	if (ce_test->area_code == area_code) {
	    if (ce_test->exchange == exchange) {
                if (ce_test->zone_local == 0) {

		    (void)printf("Free:    %d:%d/%d - %s\n",
                        current_scan_zone,
                        current_network,
                        current_node,
                        system_name);

                    return;
                }
                else {
                    (void)printf("Zone %02d: %d:%d/%d - %s\n",
                        ce_test->zone_local,
                        current_scan_zone,
                        current_network,
                        current_node,
                        system_name);

                    return;
                }
            }
        }

        ce_test = ce_test->next;
    }
}

/* **********************************************************************
   * Extract the zone, host, and node number. Then extract the area     *
   * code, exchange code, and then phone number. With that information, *
   * see what the cost is.                                              *
   *                                                                    *
   ********************************************************************** */

static char report_entry(char *this_one)
{
    char i;
    unsigned short area_code;
    unsigned short exchange;
    char system_name[81];

    if (! strncmp(this_one, "ZONE", 4)) {
        this_one += 5;
        skipspace(this_one);
	current_scan_zone = atoi(this_one);

	if (current_scan_zone != 1) {
            return(TRUE);
        }
    }
    else if (! strncmp(this_one, "HOST", 4)) {
        this_one += 5;
        skipspace(this_one);
	current_network = atoi(this_one);
    }
    else {
        while (*this_one != ',') {
            this_one++;
        }
    }

    this_one++;
    current_node = atoi(this_one);

    while (*this_one && *this_one != ',') {
        this_one++;
    }

    if (! *this_one) {
        return(FALSE);
    }

    this_one++;
    i = 0;

    while (*this_one && *this_one != ',') {
        system_name[i++] = *this_one++;
    }

    if (! *this_one) {
        return(FALSE);
    }

    this_one++;
    system_name[i] = (char)NULL;

    for (i = 0; i < 2; i++) {
        while (*this_one && *this_one != ',') {
            this_one++;
        }

        if (! *this_one) {
            return(FALSE);
        }

        this_one++;
    }

    while (*this_one && *this_one != '-') {
        this_one++;
    }

    if (! *this_one) {
        return(FALSE);
    }

    this_one++;
    area_code = atoi(this_one);

    while (*this_one && *this_one != '-') {
        this_one++;
    }

    if (! *this_one) {
        return(FALSE);
    }

    this_one++;
    exchange = atoi(this_one);
    check_entry(area_code, exchange, system_name);
    return(FALSE);
}

/* **********************************************************************
   * Go through the nodelist and produce a report.                      *
   *                                                                    *
   ********************************************************************** */

static void scan_nodelist(char *this_file)
{
    FILE *nodelist;
    char record[201], *point;

    if ((nodelist = fopen(this_file, "rt")) == (FILE *)NULL) {
        (void)printf("Nodelist file: %s could not be opened!\n", this_file);
        (void)fcloseall();
        exit(17);
    }

    while (! feof(nodelist)) {
        (void)fgets(record, 200, nodelist);

        if (! feof(nodelist)) {
            point = record;
            skipspace(point);

            if (strlen(point) > 2 && *point != ';') {
                ucase(point);

                if (report_entry(point)) {
                    (void)fclose(nodelist);
                    return;
                }
            }
        }
    }

    (void)fclose(nodelist);
}

/* **********************************************************************
   * The main entry point.                                              *
   *                                                                    *
   ********************************************************************** */

void main(int argc, char *argv[])
{
    ce_first = ce_last = ce_test = (struct Config_Entry *)NULL;
    c_first = c_last = c_test = (struct Cost_Entry *)NULL;
    cr_first = cr_last = cr_test = (struct Cost_Reduction *)NULL;

    if ((configuration = fopen("LOCAL.DAT", "rt")) == (FILE *)NULL) {
        (void)printf("Configuration file: LOCAL.DAT can be found!\n");
        exit(10);
    }

    (void)printf("%s Version %s - %s\n", __FILE__, The_Version, __DATE__);

    if (argc != 2) {
        (void)printf("\n");
        (void)printf("Enter: cost [area code and phone number] to see what it costs\n");
        (void)printf("            [Format: XXX-YYY-ZZZZ\n\n");
        (void)printf("Enter: cost [filename] to produce a report using nodelist 'filename'\n");
        (void)fclose(configuration);
        exit(11);
    }

    if (extract_configuration()) {
        if (argv[1][0] >= '0' && argv[1][0] <= '9') {
            scan_phone_number(argv[1]);
        }
        else {
            scan_nodelist(argv[1]);
        }
    }
}

