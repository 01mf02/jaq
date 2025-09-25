def pyramid($max): def rec: if . < $max then ., (.+1 | rec), . end; rec; . as $max | 0 | [pyramid($max)] | length
